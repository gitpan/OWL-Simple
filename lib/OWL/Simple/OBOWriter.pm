#!/usr/bin/perl

=head1 NAME

OWL::Simple::OBOWriter - a simple OWL to OBO converter

=head1 SYNOPSIS

	use OWL::Simple::Parser;
	use OWL::Simple::OBOWriter;
	
	# load Experimental Factor Ontology
	my $parser = OWL::Simple::Parser->new( owlfile => 'efo.owl' );
	my $writer = OWL::Simple::OBOWriter->new( owlparser => $parser );
	
	# convert the ontology to OBO and save in current directory
	$writer->write();

=head1 DESCRIPTION

A simple OWL to OBO converter.

In the constructor you only need to pass an OWL::Simple::Parser object.
All other arguments are optional:

=over

=item  outputfile

Defaults to simple-owl-obowriter-output.obo.

=item version

Version of the ontology to record in the OBO file.

=item namespace

Specifies the default namespace

=back

=head2 METHODS

=over

=item write()

Converts and writes the file in current directory

=back

=head1 AUTHOR

Tomasz Adamusiak <tomasz@cpan.org>

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2010 European Bioinformatics Institute. All Rights Reserved.

This module is free software; you can redistribute it and/or modify it 
under GPLv3.

This software is provided "as is" without warranty of any kind.

=cut

package OWL::Simple::OBOWriter;

use Moose 0.89;
use OWL::Simple::Parser 0.05;
use Log::Log4perl qw(:easy);
use XML::Parser 2.34;
Log::Log4perl->easy_init( { level => $INFO, layout => '%-5p - %m%n' } );

our $VERSION = 0.01;

has 'owlparser' => ( is => 'rw', isa => 'OWL::Simple::Parser', required => 1 );
has 'outputfile' =>
  ( is => 'rw', isa => 'Str', default => 'simple-owl-obowriter-output.obo' );
has 'version'   => ( is => 'rw', isa => 'Str', required => 0 );
has 'namespace' => ( is => 'rw', isa => 'Str', required => 0 );

sub BUILD() {
	my $self = shift;
	$self->parse_owl();
}

sub write() {
	my $self = shift;

	$self->write_header();

	$self->write_typedefs();

	$self->write_terms();
	
	INFO 'Converted ' . $self->owlparser->owlfile . ' to ' . $self->outputfile;
	
	1;
}

# Writes OBO header

sub write_header() {
	my $self = shift;
	open my $fh, '>:utf8', $self->outputfile or LOGCROAK $!;
	{
		local $\ = "\n";    # do the magic of println
		print $fh 'format-version: 1.2';
		print $fh "data-version: " . $self->version if defined $self->version;
		print $fh "date: " . datetime();
		print $fh "auto-generated-by: OWL::Simple::OBOWriter";
		print $fh "default-namespace: " . $self->namespace if defined $self->namespace;

		#print $fh "idspace: efo http://www.ebi.ac.uk/efo";
	}
	close $fh;
	DEBUG "WROTE HEADER";
}

# Writes OBO footer containing Typedef stanzas

sub write_typedefs() {
	my $self = shift;
	open my $fh, '>>:utf8', $self->outputfile or LOGCROAK $!;
	{
		local $\ = "\n";    # do the magic of println
		print $fh q{};
		print $fh '[Typedef]';    # term stanza
		print $fh 'id: part_of';
	}
	close $fh;
	DEBUG "WROTE TYPEDEFS";
}

# Initiates the OWL parser.

sub parse_owl() {
	my $self   = shift;
	my $parser = $self->owlparser;
	$parser->parse();
}

# Writes out owl classes.

sub write_terms($) {
	my $self   = shift;
	my $parser = $self->owlparser;
	open my $fh, '>>:utf8', $self->outputfile or LOGCROAK $!;

	for my $key (sort (keys %{$parser->class})) {
		my $term = $parser->class->{$key};
		local $\ = "\n";    # do the magic of println
		print $fh q{};
		print $fh '[Term]';    # term stanza
		print $fh 'id: ' . $key . ' ! ' . $term->label;
		print $fh 'name: ' . $term->label;
		print $fh 'def: ' . $term->definitions->[0]
		  if defined $term->definitions->[0];

		# write synonyms
		for my $synonym ( @{ $term->synonyms } ) {
			print $fh 'synonym: "' . escape_chars($synonym) . '" EXACT []';
		}

		# write xrefs
		for my $xref ( @{ $term->xrefs } ) {
			print $fh 'xref: ' . $xref;
		}

		# write isa_s
		for my $isa ( @{ $term->subClassOf } ) {
			$isa =~ s!http://www.ebi.ac.uk/efo/!!;
			print $fh 'is_a: ' . $isa;
		}

		# write relationships
		for my $part_of ( @{ $term->part_of } ) {
			$part_of =~ s!http://www.ebi.ac.uk/efo/!!;
			# FIXME need to fix circular references
			# print $fh 'relationship: part_of ' . $part_of;
		}
	}

	close $fh;
	DEBUG "WROTE TERMS";
}

# Supplies a date in an OBO required format.

sub datetime() {
	my ( $sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst ) =
	  localtime(time);
	return sprintf "%02d:%02d:%4d %02d:%02d", $mon + 1, $mday, $year + 1900,
	  $hour, $min;
}

# Escape chars in synonyms and definitions.

sub escape_chars($) {
	my $s = shift;
	$s =~ s/\[/\\\[/g;
	$s =~ s/\]/\\\]/g;
	$s =~ s/"//g;
	return $s;
}

1;
