#define VARIABLE(name) name
#define NESTED_VARIABLE(kk) VARIABLE(kk)
#define CONDI NESTED_VARIABLE(var) < 10
#define METHOD(ret, rolf) bool rolf##_w() { int x; return ret; } bool n() { return false; }
#define CLASS(name) class name    {      };
//class Sowiso {
//        METHOD(true, Sowiso)
//};
METHOD(true, lo)
CLASS(hello)
/*bool looping() {
	int var = 0;
	int vak = 0;
        return NESTED_VARIABLE(vak);
}*/
