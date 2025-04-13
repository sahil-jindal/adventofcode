package org.example;

final class AdditionOperation extends Operation {
    AdditionOperation(int number) {
        super(number);
    }

    @Override
    int operate(int old) {
        return old + this.constantNumber;
    }
}
