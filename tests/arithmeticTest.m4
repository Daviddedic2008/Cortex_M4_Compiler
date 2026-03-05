# test constant folding #
word fold = 5 + 6 * 7 / 2;
word fold2 = 5 + 7 + fold;

# test basic integer arithmetic #
word x = 1; word y = 2;
word z = x * y + x + y / x; # here, lazy register storage/repurposing is visible #

# test multi-word arithmetic #
word 2 x2 = 10; word 2 y2 = 20;
word 2 z2 = x2 + y2;

# test signed values #
word x3 = -1; word y3 = 5;
word z3 = x3 * y3 - x3; 