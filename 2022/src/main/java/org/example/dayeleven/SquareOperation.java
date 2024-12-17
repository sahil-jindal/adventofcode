package org.example;

public class SquareOperation extends Operation {
    public SquareOperation() {
        super(0);
    }

    @Override
    int operate(int old) {
        return old * old;
    }
}
