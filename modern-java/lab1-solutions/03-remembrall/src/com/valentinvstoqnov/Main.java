package com.valentinvstoqnov;

import static com.valentinvstoqnov.Remembrall.isPhoneNumberForgettable;

public class Main {

    public static void main(String[] args) {
        System.out.println(isPhoneNumberForgettable(""));
        System.out.println(isPhoneNumberForgettable("498-123-123"));
        System.out.println(isPhoneNumberForgettable("0894 123 567"));
        System.out.println(isPhoneNumberForgettable("(888)-FLOWERS"));
        System.out.println(isPhoneNumberForgettable("(444)-greens"));
    }
}
