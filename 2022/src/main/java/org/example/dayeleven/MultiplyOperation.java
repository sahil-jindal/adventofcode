package org.example;

public class MultiplyOperation extends Operation {

    MultiplyOperation(int number) {
        super(number);
    }

    @Override
    int operate(int old) {
        return old * constantNumber;
    }
}
