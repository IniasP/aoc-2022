#!/usr/bin/env perl
use strict;
use warnings;

open(my $in, "<", "input");
my @lines = <$in>;

sub sgn {
    my $n = shift;
    if ($n > 0) {
        return 1;
    } elsif ($n < 0) {
        return -1;
    } else {
        return 0;
    }
}

sub move_in_dir {
    my ($pos, $direction) = @_;
    my ($x, $y) = split(",", $pos);
    if ($direction eq "U") {
        ++$y;
    } elsif ($direction eq "D") {
        --$y;
    } elsif ($direction eq "L") {
        --$x;
    } elsif ($direction eq "R") {
        ++$x;
    }
    return "$x,$y";
}

sub update_tail {
    my ($head, $tail) = @_;
    my ($hx, $hy) = split(",", $head);
    my ($tx, $ty) = split(",", $tail);
    my $x_diff = $hx - $tx;
    my $y_diff = $hy - $ty;
    if (abs($x_diff) > 1) {
        $tx += sgn($x_diff);
        $ty += sgn($y_diff) if ($hy != $ty);
    } elsif (abs($hy - $ty) > 1) {
        $ty += sgn($y_diff);
        $tx += sgn($x_diff) if ($hx != $tx);
    }
    return "$tx,$ty";
}

sub solve {
    my $n = shift;
    my @rope = ("0,0") x $n;
    my %visited = (
        "0,0" => 1
    );
    foreach(@lines) {
        my ($direction, $n) = split(" ", $_);
        foreach (1 .. $n) {
            $rope[0] = move_in_dir($rope[0], $direction);
            foreach my $i (1 .. $#rope) {
                $rope[$i] = update_tail($rope[$i - 1], $rope[$i]);
            }
            my ($tx, $ty) = split(",", $rope[-1]);
            my $tail_pos = "$tx,$ty";
            if (exists $visited{$tail_pos}) {
                ++$visited{$tail_pos};
            } else {
                $visited{$tail_pos} = 1;
            }
        }
    }
    return keys %visited;
}

my $sol1 = solve(2);
print "Part 1: $sol1\n";

my $sol2 = solve(10);
print "Part 2: $sol2\n";
