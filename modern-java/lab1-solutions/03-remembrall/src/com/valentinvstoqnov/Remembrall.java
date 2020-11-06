package com.valentinvstoqnov;

public class Remembrall {

    public static boolean isPhoneNumberForgettable(String phoneNumber) {
        if (phoneNumber == null || phoneNumber.isEmpty()) {
            return false;
        }

        boolean hasRepetitiveDigits = false;
        boolean hasNonDigit = false;
        boolean[] repetitiveDigits = new boolean[10];
        for (int i = 0; i < phoneNumber.length(); ++i) {
            final char ch = phoneNumber.charAt(i);
            if (Character.isDigit(ch)) {
                int digit = ch - '0';
                if (repetitiveDigits[digit]) {
                    hasRepetitiveDigits = true;
                } else {
                    repetitiveDigits[digit] = true;
                }
            } else if (ch != ' ' && ch != '-') {
                hasNonDigit = true;
            }
        }

        return !hasRepetitiveDigits || hasNonDigit;
    }
}
