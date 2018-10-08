#pragma once

template<typename F>
struct my_defer {
    F _f;
    my_defer(F f) : _f(f) {}
    ~my_defer() { _f(); }
};

template <typename F>
my_defer<F> defer_func(F f) {
    return my_defer<F>(f);
}

#define DEFER_1(x, y) x##y
#define DEFER_2(x, y) DEFER_1(x, y)
#define DEFER_3(x)    DEFER_2(x, __COUNTER__)
#define defer(code)   auto DEFER_3(_defer_) = defer_func([&](){code;})