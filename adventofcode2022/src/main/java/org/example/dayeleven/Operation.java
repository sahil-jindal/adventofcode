package org.example;

abstract class Operation {
    protected int constantNumber;
    public Operation(int number) {
        this.constantNumber = number;
    }
    abstract int operate(int old);
}
