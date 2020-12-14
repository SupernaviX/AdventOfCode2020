use strict;
use warnings;
use POSIX;
use List::Util 'first', 'min';

sub get_input
{
  open(my $fd, '<', 'input') or die $!;
  my $start = <$fd>;
  my @ids = split(',', <$fd>) or die $!;
  my @array;
  for my $id ( @ids ) {
    if ($id ne 'x') {
      push @array, { id => $id }
    }
  }
  return ($start, @array);
}

sub next_departure
{
  my $time = $_[0];
  my $period = $_[1];
  return ceil($time / $period) * $period;
}

(my $start, my @array) = get_input();
for my $item ( @array ) {
  my $id = $item->{id};
  my $time = next_departure($start, $id);
  $item->{time} = $time;
}
my $fastest_time = min map { $_->{time} } @array;
my $fastest = first { $_->{time} == $fastest_time } @array;
my $answer = $fastest->{id} * ($fastest_time - $start);
print("$answer\n");
