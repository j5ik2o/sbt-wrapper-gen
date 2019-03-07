package example;

import javax.annotation.Nonnull;

public class Customer {

    private String firstName;
    private String lastName;

    public Customer(String firstName, String lastName) {
        this.firstName = firstName;
        this.lastName = lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    @Nonnull
    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }
}
