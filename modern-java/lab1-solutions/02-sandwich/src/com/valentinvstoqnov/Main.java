package com.valentinvstoqnov;

import static com.valentinvstoqnov.SandwichExtractor.extractIngredients;

public class Main {

    private static void printIngredients(String[] ingredients) {
        for (String ingredient : ingredients) {
            System.out.print(ingredient + " ");
        }
        System.out.println();
    }

    public static void main(String[] args) {
        printIngredients(extractIngredients("asdbreadham-tomato-mayobreadblabla"));
        printIngredients(extractIngredients("asdbreadham-olives-tomato-olives-mayobreadblabla"));
        printIngredients(extractIngredients("asdbreadham"));
    }
}
