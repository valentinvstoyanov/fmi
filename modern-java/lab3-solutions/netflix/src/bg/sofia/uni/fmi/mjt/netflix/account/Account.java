package bg.sofia.uni.fmi.mjt.netflix.account;

import java.time.LocalDateTime;
import java.time.Period;

public class Account {

    private final String username;
    private final int age;

    public Account(final String username, final LocalDateTime birthdayDate) {
        this.username = username;
        this.age = calculateAge(birthdayDate);
    }

    private int calculateAge(final LocalDateTime birthdayDate) {
        return Period.between(birthdayDate.toLocalDate(), LocalDateTime.now().toLocalDate()).getYears();
    }

    public String getUsername() {
        return username;
    }

    public int getAge() {
        return age;
    }
}
