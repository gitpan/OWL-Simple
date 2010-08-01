use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'OWL-Simple',
    license             => 'gpl',
    dist_author         => q{Tomasz Adamusiak <tomasz@ebi.ac.uk>},
    dist_version => '0.4',
    dist_abstract => 'A set of modules and scripts for parsing OWL ontologies',
    requires => {
        'Test::More' => 0,
		'Class::Std' => 0,
		'IO::File' => 0,
		'IO::Handle' => 0,
		'GO::Parser' => 0,
		'Getopt::Long' => 0,
		'Moose' => '0.89',
		'Log::Log4perl' => 0,
		'XML::Parser' => '2.34',
		'Benchmark' => 0,
		'Text::LevenshteinXS' => 0,
    },
    #script_files        => ['scripts/similarity_match.pl'],
    add_to_cleanup      => [ 'OWL-Simple-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
