#!/usr/bin/env perl
package OWL::Simple::Class;

use Moose 0.89;

=head1 NAME

OWL::Simple::Class

=head1 DESCRIPTION

Helper class to store information for a single owl:Class parsed by 
the OWL::Simple::Parser. Not to be used directly.

Exposes label, id and a synonyms array.

=head1 AUTHOR

Tomasz Adamusiak 2010

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2010 European Bioinformatics Institute. All Rights Reserved.

This module is free software; you can redistribute it and/or modify it 
under GPLv3.

This software is provided "as is" without warranty of any kind.

=cut

our $VERSION = 0.04;

has 'label' => ( is => 'rw', isa => 'Str' );
has 'synonyms' => ( is => 'ro', isa => 'ArrayRef', default => sub { [] } );
has 'definitions' => ( is => 'ro', isa => 'ArrayRef', default => sub { [] } );
has 'xrefs' => ( is => 'ro', isa => 'ArrayRef', default => sub { [] } );
has 'subClassOf' => ( is => 'ro', isa => 'ArrayRef', default => sub { [] } );
has 'part_of' => ( is => 'ro', isa => 'ArrayRef', default => sub { [] } );
has 'id' => (
	is      => 'rw',
	isa     => 'Str',
	trigger => sub {
		my ( $self, $id ) = @_;
		$self->{id} =~ s!http://www.ebi.ac.uk/efo/!!; # strip the efo namespace from id
	}
);

1;

=head1 AUTHOR

Tomasz Adamusiak 2010

=cut