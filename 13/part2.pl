use strict;
use warnings;
use POSIX;
use List::Util 'first', 'min';
use Math::BigInt 'bgcd';

sub get_input
{
  open(my $fd, '<', 'input') or die $!;
  my $start = <$fd>;
  my @ids = split(',', <$fd>) or die $!;
  my @array;
  for my $i ( 0..$#ids ) {
    my $id = $ids[$i];
    if ($id ne 'x') {
      my $offset = $id - $i;
      while ($offset < 0) {
        $offset += $id;
      }
      push @array, { id => $id, offset => $offset }
    }
  }
  return ($start, @array);
}

sub lcm
{
  my ($x, $y) = @_;
  return ($x * $y) / bgcd($x, $y)->numify;
}

sub intersection
{
  my ($period1, $offset1, $period2, $offset2) = @_;
  my $offset = $offset1;
  while ($offset % $period2 != $offset2) {
    $offset += $period1;
  }
  my $period = lcm($period1, $period2);
  return ($period, $offset);
}

my ($start, @array) = get_input();
my $total_period = $array[0]->{id};
my $total_offset = $array[0]->{offset};
for my $item ( @array[1 .. $#array] ) {
  my $id = $item->{id};
  my $offset = $item->{offset};
  ($total_period, $total_offset) = intersection($total_period, $total_offset, $id, $offset);
}
print "$total_offset\n";
