package example;

import javax.annotation.Nonnull;

public class Customer {
    private final String firstName;
    private final String lastName;

    public Customer(String firstName, String lastName) {
        this.firstName = firstName;
        this.lastName = lastName;
    }

    @Nonnull
    public String getFirstName() {
        return firstName;
    }

    @Nonnull
    public String getLastName() {
        return lastName;
    }
}
