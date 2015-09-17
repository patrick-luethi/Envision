#define VARIABLE(name) name
#define NESTED_VARIABLE(kk) VARIABLE(kk)
#define CONDI NESTED_VARIABLE(var) < 10
#define METHOD(ret, rolf) bool rolf##_w() { int x; return ret; } bool n() { return false; }
#define CLASS(name) class name    { \
			static int name##Index_; \
			METHOD(nullptr, name##_Method)     \
			};
#define PARTIAL_BEGIN(name) class name    { int name##Index_;
#define PARTIAL_END  METHOD(nullptr, name##_Method)     };
#define STATEMENTS if (3 == 0) return; return;

class Sowiso {
		  METHOD(true, Sowiso)

		  void tra()
		  {
			  STATEMENTS
		  }
};

CLASS(hello)

//PARTIAL_BEGIN(hi)
//PARTIAL_END

/*bool looping() {
	int var = 0;
	int vak = 0;
        return NESTED_VARIABLE(vak);
}*/
