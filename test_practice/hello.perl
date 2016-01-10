#!/usr/bin/perl
#Alejandro Aguilar aaguil10@ucsc.edu
use strict;
use warnings;

# print "hello hello now!\n";

# my $chicken = "pollo";
# my $age = 22;

# print $chicken;
# print "\n";

# #arrays
# my @barn = ($chicken, "Cow", "pig");
# my @number = (44,44,22,13);
# print $barn[-1]. "\n";

# #hashes
# my %fruit_color = ("apple", "red", "banana", "yellow");
# print $fruit_color{"apple"} . "\n";

# #Reg exp
# my $regString = "Alejandro Ana Jim Mary lamb Jesus Jim Lazarith Damien Coriline";

# if($regString =~ /Ana/m) {
# 	print $_;

# }
# while($regString =~ s/Jim\W/Jimmy /){
# 	print $regString;
# 	print "\n";
# }

# open(my $in,  "<",  "emails.txt")  or die "Can't open input.txt: $!";

# my @myarry = ();
# while (my $row = <$in>) {
#   chomp $row;
#   push (@myarry, $row); 
#   print "$row\n";
# }

# for my $e (@myarry){
# 	print ": " . $e . "\n";
# }

# logger("debug", "OHHHH");

# sub logger {
# 	my $tag = shift;
# 	my $msg = shift;
# 	print $tag . ": " . $msg . "\n";

# }

# my $line;
# my %count;
# my @array;

# while ($line = <>) {
#    #++$count{$_} for split m/\W+/, $line;
#    print "$_ \n" for split m/\W+/, $line;
# }

# push @array, sprintf "%10d %s\n", $count{$_}, $_ for keys %count;
# @array = sort { $b cmp $a } @array;
# print $array[$_] for 0..9;

# my $lc;
# my $cc;
# my $wc;

# $lc++, $cc+= length, $wc+= @{[m/(\S+)/g]} while <>;
# print "$lc $wc $cc\n";

# $obj =  = {
#          NAME      => $string,
#          CMDS  => [ @old_values ],
#      };
#      print $obj->{TEXT};
#      print $rec->{SEQUENCE}[0];
#      $last = pop @ { $rec->{SEQUENCE} };

my %taco;
my $node = {
	PREREC => '$',
	CMDS => '@'
};

my $prerec = 'PREREC';

$node->{$prerec} = "letuce cheese tomato";
$node->{'CMDS'} = ["cook", "eat", "throw" ];

#push @{$node->CMDS}, 1;
$taco{"all"} = $node;


print $taco{"all"} . "\n";

print $node->{$prerec};