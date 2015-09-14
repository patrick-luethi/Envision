#define VARIABLE(name) name + 1
#define NESTED_VARIABLE(kk) VARIABLE(kk)
#define CONDI NESTED_VARIABLE(var) < 10
#define METHOD(ret, rolf) bool m##rolf() { return ret; } bool n() { return false; }

/*class Sowiso {
        METHOD(true, _megaServer)
};*/

bool looping() {
	int var = 0;
	int vak = 0;
        return NESTED_VARIABLE(vak);
}
