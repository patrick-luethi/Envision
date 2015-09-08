#define VARIABLE(rofl, name) name
#define NESTED_VARIABLE VARIABLE(lol, var)
#define CONDI NESTED_VARIABLE < 10

bool looping() {
	int var = 0;
	return CONDI;
}
