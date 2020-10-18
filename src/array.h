#pragma once

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <initializer_list>

template<typename T> struct Array {

    Array(uint32_t initial_capacity = 100): _size(0), _capacity(initial_capacity) {
        assert(initial_capacity > 0);
        _data = (T*) malloc(sizeof(T) * initial_capacity);
    }

    Array(std::initializer_list<T> initializer_list) {
        _size = initializer_list.size();
        _capacity = _size;
        _data = (T*) malloc(sizeof(T) * _capacity);
        auto pointer = initializer_list.begin();
        for (uint32_t i = 0; i < _size; ++i) {
            new (&_data[i]) T(*pointer++);
        }
    }

    ~Array() {
        for (uint32_t i = 0; i < _size; ++i) {
            _data[i].~T();
        }
        free(_data);
    }

    Array(const Array& other) {
        _size = other._size;
        _capacity = other._capacity;
        _data = (T*) malloc(sizeof(T) * _capacity);
        for (uint32_t i = 0; i < _size; ++i) {
            new (&_data[i]) T(other._data[i]);
        }
    }

    Array(Array&& other) {
        _size = other._size;
        _capacity = other._capacity;
        _data = other._data;
        other._size = 0;
        other._capacity = 0;
        other._data = nullptr;
    }

    Array& operator= (const Array& other) {
        for (uint32_t i = 0; i < _size; ++i) {
            _data[i].~T();
        }
        _size = other._size;
        _capacity = other._capacity;
        _data = (T*) malloc(sizeof(T) * _capacity);
        for (uint32_t i = 0; i < _size; ++i) {
            new (&_data[i]) T(other._data[i]);
        }
        return *this;
    }

    Array& operator= (Array &&) {
        _size = other._size;
        _capacity = other._capacity;
        _data = other._data;
        other._size = 0;
        other._capacity = 0;
        other._data = nullptr;
        return *this;
    }

    T& operator[](int index) {
        assert(index >= 0 && index < _size);
        return _data[index];
    }

    T operator[](int index) const {
        assert(index >= 0 && index < _size);
        return _data[index];
    }

    void ensure_space(uint32_t required_space) {
        if (required_space <= _capacity) {
            return;
        }
        _capacity *= 2;
        if (required_space > _capacity) {
            _capacity = required_space;
        }
        T* new_data = (T*) malloc(sizeof(T) * _capacity);
        for (uint32_t i = 0; i < _size; ++i) {
            new_data[i] = _data[i];
            _data[i].~T();
        }
        free(_data);
        _data = new_data;
    }

    void push(const T& element) {
        ensure_space(_size + 1);
        new (&_data[_size++]) T(element);
    }

    void push(T&& element) {
        ensure_space(_size + 1);
        new (&_data[_size++]) T(element);
    }

    T pop() {
        assert(_size > 0);
        T copy = _data[_size - 1];
        _data[--_size].~T();
        return copy;
    }

    T back() const {
        return _data[_size - 1];
    }

    T& back() {
        return _data[_size - 1];
    }

    bool empty() const {
        return _size == 0;
    }

    uint32_t size() const {
        return _size;
    }

    T* data() {
        return _data;
    }

    T* begin() const { return &_data[0]; }
    T* end() const { return &_data[_size]; }

private:
    T* _data;
    uint32_t _size;
    uint32_t _capacity;
};