#!/usr/bin/env perl

=head1 NAME

similarity_match.pl

=head1 SYNOPSIS

Compares a list of annotations to another ontology and suggests the best match 
based on some similarity metric (Levenshtein distance). It is also possible to 
align one ontology to another. Accepts ontologies in both OBO and OWL formats 
as well as MeSH ASCII.

The script runs non-interactively and the results have to be manually inspected, 
although it can be expected that anything with a similarity score higher 
than ~80-90% will be a valid match.  

=head2 USAGE

similarity_match.pl (-w owlfile || -o obofile || -m meshfile) 
					-t targetfile -r resultfile 
					[--obotarget || --owltarget]

Optional '--obotarget' setting specifies that the target file is an OBO ontology.
Optional '--owltarget' setting specifies that the target file is an OWL ontology.

=head2 INPUT FILES

=over

=item ontologies to map the targetfile against

owlfile, obofile, meshfile are ontologies in OWL, OBO and MeSH ASCII formats. 
Only a single file needs to be specified.

=item targetfile

The script expects a single column text file with no headears.

=back

=head2 OUTPUT

The script will produce a single tab-delimited file as set with the
-r flag. The file will have four headers:

=over

=item ID 

Accession of the term from the targetfile if the file was an ontology, 
otherwise OE_VALUE repeated.

=item OE_VALUE

Annotation from the supplied targetfile or a term label if the file
was an ontology. 

=item ONTOLOGY_TERM

Term label that was matched based on the highest similarity from
the supplied onotlogy file.

=item ACCESSION

Accession of the ontology term that provided the best match.

=item SIMILARITY%

Similarity score of ONTOLOGY_TERM compared to OE_VALUE. 
This is the Levenshtein distance normalised by OE_VALUE
length expressed in %. Higher is better.

=back

=cut

use lib 'C:\strawberry\perl\site\lib';

use strict;
use warnings;

use IO::File;
use Getopt::Long;

use GO::Parser;
use OWL::Simple::Parser 0.06;
use MeSH::Parser::ASCII 0.02;
use Log::Log4perl qw(:easy);
use IO::Handle;
use Benchmark ':hireswallclock';

use Text::LevenshteinXS;

use Data::Dumper;

#use Text::WagnerFischer qw(distance);

Log::Log4perl->easy_init( { level => $INFO, layout => '%-5p - %m%n' } );

# script arguments
my (
	$owlfile,   $obofile,   $targetfile, $resultfile,
	$obotarget, $owltarget, $meshfile
);
my $flat_header = undef;
my @flat_header;

sub main() {

	# initalize

	GetOptions(
		"o|obofile=s"  => \$obofile,
		"w|owlfile=s"  => \$owlfile,
		"m|meshfile=s" => \$meshfile,
		"t|target=s"   => \$targetfile,
		"r|results=s"  => \$resultfile,
		"obotarget"    => \$obotarget,
		"owltarget"    => \$owltarget,
	);

	usage()
	  unless ( $owlfile || $obofile || $meshfile )
	  && $targetfile
	  && $resultfile;

	# load appropriate files
	my ( $ontology, $data );
	$ontology = parseOWL($owlfile)   if defined $owlfile;
	$ontology = parseOBO($obofile)   if defined $obofile;
	$ontology = parseMeSH($meshfile) if defined $meshfile;
	$data     = parseFlat($targetfile)
	  unless defined $obotarget || defined $owltarget;
	$data = parseOBO($targetfile) if defined $obotarget;
	$data = parseOWL($targetfile) if defined $owltarget;

	normalise_hash($ontology);
	check_data($data);

	#print Dumper( \%term );

	process_and_write( $data, $ontology, $resultfile );
}

sub usage() {
	print(<<"USAGE");

similarity_match.pl (-w owlfile || -o obofile || -m meshfile) 
					-t targetfile -r resultfile 
					[--obotarget || --owltarget]

Optional '--obotarget' setting specifies that the target file is an OBO ontology
Optional '--owltarget' setting specifies that the target file is an OWL ontology
USAGE
	exit 255;
}

=head1 DESCRIPTION

=head2 Function list

=over

=item normalise_hash()

Normalises labels and synonyms in the target hash. These
are stored in extra annotations on the hash, so that the
original value is preserved for display.

=cut

sub normalise_hash($) {
	my $hash = shift;

	for my $id ( keys %$hash ) {
		my $label = $hash->{$id}->{label};
		$hash->{$id}->{normalised_label} = normalise($label);

		for my $synonym ( @{ $hash->{$id}->{synonyms} } ) {
			$hash->{$id}->{normalised_syns_hash}->{$synonym} =
			  normalise($synonym);
		}
	}
}

=item check_data()

Checks the input data, e.g. removing empty lines or warning of duplicates

=cut

sub check_data($) {
	my $hash = shift;

	for my $id ( keys %$hash ) {
		my $label = $hash->{$id}->{label};
		if ( $label ne '' ) {
			my $synonyms_checked;
			for my $synonym ( @{ $hash->{$id}->{synonyms} } ) {
				if ( $synonym ne '' ) {
					push @$synonyms_checked, $synonym;
				}
				else {
					WARN 'Empty synonym detected in input';
				}
				$hash->{$id}->{synonyms} = $synonyms_checked;
			}
		}
		else {
			WARN 'Empty line detected in input';
			delete $hash->{$id};
		}
	}
}

=item normalise()

Normalises a string by changing it lowercase and
sorting all the words within a phrase splitting
on non-alphanumerics.

=cut

sub normalise($) {
	my $word = shift;

	# lowercase has to be done before compare
	$word = lc($word);

	# split on non-alphanumeric chars, sort words, join
	#$word = join( q{}, sort { $a cmp $b } ( split /[[:^alnum:]]/, $word ) );

	# sort characters alphabetically
	$word = join '', sort split( '', $word );
}

=item align()

Aligns the two data structures targetfile and ontology. Outputs
the results into a file.

=cut

sub process_and_write($$$) {
	my ( $data, $ontology, $file ) = @_;
	open my $fh_out, '>', $file;
	$fh_out->autoflush(1);

	# write header
	if ( defined $obotarget || defined $owltarget ) {
		print $fh_out "ID\tOE_VALUE\t";
	}
	else {
		print $fh_out $flat_header[0] . "\t";
	}
	print $fh_out "ONTOLOGY_TERM\tACCESSION\tSIMILARITY%";
	print $fh_out "\t$flat_header[1]" if defined $flat_header;
	print $fh_out "\n";

	my $c;
	my $t0 = new Benchmark;

	for my $id ( keys %$data ) {
		$c++;
		my $label = $data->{$id}->{label};

		# do not output id for flat files as it's same as label
		print $fh_out $id . "\t"
		  if defined $obotarget || defined $owltarget;

		# output match info
		print $fh_out find_match( $ontology, $label );

		# output unprocessed columns back
		print $fh_out "\t" . $data->{$id}->{ragged_end};

		# line ending
		print $fh_out "\n";

		# this only happens for owl or obo targets
		for my $synonym ( @{ $data->{$id}->{synonyms} } ) {
			print $fh_out $id . "\t" . find_match( $ontology, $synonym ) . "\n";
			$c++;
		}
		INFO "Processed " . $c
		  if $c % 100 == 0;
	}

	my $t1 = new Benchmark;
	INFO "Processed $c elements in " . timestr( timediff( $t1, $t0 ) );
	close $fh_out;
}

=item parseMeSH()

Custom MeSH parser for the MeSH ASCII format.

=cut

sub parseMeSH($) {
	my ($file) = @_;
	my $term;
	INFO "Parsing MeSH file $file ...";

	my $parser = MeSH::Parser::ASCII->new( meshfile => $file );

	# parse the file
	$parser->parse();

	# loop through all the headings
	while ( my ( $id, $heading ) = each %{ $parser->heading } ) {
		print $id . ' - ' . $heading->{label} . "\n";

		$term->{$id}->{label}    = $heading->{label};
		$term->{$id}->{synonyms} = $heading->{synonyms};
	}

	return $term;
}

=item parseFlat()

Custom flat file parser.

=cut

sub parseFlat($) {
	my $file = shift;
	my $term;
	INFO "Parsing flat file $file ...";

	open my $fh_in, '<', $file;

	# parse header
	$flat_header = <$fh_in>;
	chomp($flat_header);
	@flat_header = $flat_header =~ /(.*?)\t(.*)/;
	INFO "Using first line as header <$flat_header>";
	INFO "Using first column <$flat_header[0]> to match terms";

	# load input
	while (<$fh_in>) {
		chomp;

		# preserve existing columns in the file
		my ( $label, $ragged_end ) = /(.*?)\t(.*)/;

		# trim
		$label =~ s/^\s+//;
		$label =~ s/\s+$//;

		# drop trailing quotation marks (excel artefact?)
		$label =~ s/^"+//;
		$label =~ s/"+$//;

		WARN "Duplicated <$label>" if exists $term->{$label};
		$term->{$label}->{label}      = $label;
		$term->{$label}->{ragged_end} = $ragged_end;
	}

	close $fh_in;
	my $data_size = scalar keys %$term;
	INFO "Loaded $data_size unique strings";

	return $term;
}

=item parseOBO()

Custom OBO parser.

=cut

sub parseOBO($) {
	my $file = shift;
	my $term;
	INFO "Parsing obo file $file ...";
	my $parser = new GO::Parser( { handler => 'obj' } );
	$parser->parse($file);
	my $graph = $parser->handler->graph();

	# load terms into hash
	my $class_count;
	my $synonym_count;

	for my $OBOclass ( @{ $graph->get_all_terms() } ) {
		if ( $OBOclass->is_obsolete ) {
			INFO $OBOclass->public_acc() . ' obsoleted';
			next;
		}
		$class_count++;
		$synonym_count += scalar( @{ $OBOclass->synonym_list() } );

		$term->{ $OBOclass->public_acc() }->{label} = $OBOclass->name();
		$term->{ $OBOclass->public_acc() }->{synonyms} =
		  $OBOclass->synonym_list()
		  if defined @{ $OBOclass->synonym_list() };
	}

	INFO "Loaded "
	  . $class_count
	  . " classes and "
	  . $synonym_count
	  . " synonyms";

	return $term;
}

=item parseOWL()

Custom OWL parser.

=cut

sub parseOWL($) {
	my ($file) = @_;
	my $term;
	INFO "Parsing owl file $file ...";
	my $parser;

	# invoke parser
	$parser = OWL::Simple::Parser->new( owlfile => $file );

	# parse file
	$parser->parse();

	while ( my ( $id, $OWLClass ) = each %{ $parser->class } ) {
		if ( $OWLClass->label =~ /obsolete/ ) {
			next;
		}
		$term->{$id}->{label}    = $OWLClass->label;
		$term->{$id}->{synonyms} = $OWLClass->synonyms
		  if defined $OWLClass->synonyms;
	}
	return $term;
}

=item find_match()

A wrapper around the calculate_distance function. Specifies the
similarity metric to be used, in this case Text::LevenshteinXS::distance.

Outputs a single line in the output file.

=cut

sub find_match($$) {
	my ( $ontology, $term_to_match ) = @_;

	my $distance = calculate_distance(
		$ontology,
		$term_to_match,
		sub($$) {
			my ( $word1, $word2 ) = @_;
			return Text::LevenshteinXS::distance( $word1, $word2 );
		}
	);

	my $matched_term = $distance->{term};
	my $type         = $distance->{type};
	my $matched_acc  = $distance->{acc};
	my $similarity   = $distance->{sim};

	$matched_term =
	  "SYN: $matched_term OF: " . $ontology->{$matched_acc}->{label}
	  if $type eq "synonym";

	my $output_str =
	    $term_to_match . "\t"
	  . $matched_term . "\t"
	  . $matched_acc . "\t"
	  . $similarity;

	#INFO $output_str;
	return $output_str;
}

=item calculate_distance()

Finds the best match for the supplied term in the ontology
using the supplied anonymous distance function defined in
find_match().

=cut

sub calculate_distance($$&) {
	my ( $ontology, $term_to_match, $distance_function ) = @_;
	my $matched_term;
	my $matched_acc;
	my $type;
	my $min_distance = undef;
	my $similarity;

	# ontology hash was already prenormalised earlier
	my $normalised_term = normalise($term_to_match);

	for my $id ( keys %$ontology ) {
		my $label            = $ontology->{$id}->{label};
		my $normalised_label = $ontology->{$id}->{normalised_label};
		my $distance =
		  $distance_function->( $normalised_term, $normalised_label );
		if ( !( defined $min_distance ) || $distance < $min_distance ) {
			$min_distance = $distance;
			$matched_term = $label;
			$matched_acc  = $id;
			$type         = "label";
		}

		for my $synonym ( keys %{ $ontology->{$id}->{normalised_syns_hash} } ) {
			my $normalised_syn =
			  $ontology->{$id}->{normalised_syns_hash}->{$synonym};
			my $distance =
			  $distance_function->( $normalised_term, $normalised_syn );
			if ( !( defined $min_distance ) || $distance < $min_distance ) {
				$min_distance = $distance;
				$matched_term = $synonym;
				$matched_acc  = $id;
				$type         = "synonym";
			}
		}
	}

	if ( lc($matched_term) eq lc($term_to_match) ) {    # perfect match
		$similarity = 100;
	}
	else {

		# normalise the minimal LD distance
		$similarity = int(
			(
				( length($term_to_match) - $min_distance ) /
				  length($term_to_match)
			) * 100
		);
		if ( $similarity == 100 ) {
			WARN
"Possible anagram mismatch, mapping <$term_to_match> to <$matched_term> at at 99% similarity";
			$similarity = 99;    # make sure it's eyballed at some point
		}
	}

	return {
		term => $matched_term,
		acc  => $matched_acc,
		sim  => $similarity,
		type => $type,
	};
}

=back

=cut

=head1 AUTHORS

Tomasz Adamusiak <tomasz@cpan.org>

=cut

main();
