package example;

import javax.annotation.Nonnull;

public class Customer {

    private String firstName;
    private String lastName;
    private CustomerType customerType;

    public Customer(String firstName, String lastName, CustomerType customerType) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.customerType = customerType;
    }

    public void setLastName(String lastName) throws IllegalArgumentException {
        this.lastName = lastName;
    }

    @Nonnull
    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public CustomerType getCustomerType() {
        return customerType;
    }
}
