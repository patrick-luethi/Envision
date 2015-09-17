#define DECLARE static int field;
#define DEFINE int C::field = 1;

class C
{
		DECLARE
};

DEFINE
