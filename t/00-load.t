#!perl -T

use Test::More tests => 2;

BEGIN {
    use_ok( 'OWL::Simple::Parser' ) || print "Bail out!
";
    use_ok( 'OWL::Simple::Class' ) || print "Bail out!
";
}

diag( "Testing OWL::Simple::Parser $OWL::Simple::Parser::VERSION, Perl $], $^X" );
