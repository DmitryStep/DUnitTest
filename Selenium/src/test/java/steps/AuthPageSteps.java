package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.��;
import org.openqa.selenium.support.ui.ExpectedConditions;
import pages.AuthPage;

public class AuthPageSteps extends BaseSteps {

    private AuthPage _authPage = null;

    //----------------------------------------- Hooks -----------------------------------------------------

    @Before(order=1)
    public void beforeTest(){
        _authPage = new AuthPage(_driver);
    }

    @After(order=1)
    public void afterTest() {
        AfterTest();
        _authPage = null;
    }


    //----------------------------------------- Steps -----------------------------------------------------

    @����("������ ����� (.*)")
    @When("Type login (.*)")
    public void TypeLogin(String login) {
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        _authPage.typeLogin(login);
    }

    @����("������ ������ (.*)")
    @When("Type password (.*)")
    public void TypePassword(String password) {
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        _authPage.typePassword(password);
    }

    @����("������ ����� � �������")
    @When("Click Login to the system")
    public void ClickAuthButton() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.authButton()));
        _authPage.clickAuthButton();
    }

    @����("������������ ����������� � ������� (.*) � ������� (.*)")
    @Given("User authorized with login (.*) and password (.*)")
    @����("^�������������� � ������� (.*) � ������� (.*)$")
    @When("^Authorize with login (.*) and password (.*)$")
    public void Authorize(String login, String password){
        TypeLogin(login);
        TypePassword(password);
        ClickAuthButton();
    }

    //--------------------------------------- Assertions --------------------------------------------------

    @��("����� = (.*)")
    @Then("Login = (.*)")
    public void AssertLogin(String ExpectedLogin){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        assertEquals(ExpectedLogin, _authPage.loginTextField().getText());
    }

    @��("����� ������")
    @Then("Login is empty")
    public void AssertLoginIsEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        assertEquals("", _authPage.loginTextField().getText());
    }

    @��("����� �� ������")
    @Then("Login is not empty")
    public void AssertLoginIsNotEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        assertNotEquals("", _authPage.loginTextField().getText());
    }

    @��("������ = (.*)")
    @Then("Password = (.*)")
    public void AssertPassword(String ExpectedPassword){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        assertEquals(ExpectedPassword, _authPage.passwordTextField().getText());
    }

    @��("������ ������")
    @Then("Password is empty")
    public void AssertPasswordIsEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        assertEquals("", _authPage.passwordTextField().getText());
    }

    @��("������ �� ������")
    @Then("Password is not empty")
    public void AssertPasswordIsNotEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        assertNotEquals("", _authPage.passwordTextField().getText());
    }

    @��("�������� ��������� (.*)")
    @Then("Message (.*) was showed")
    public void AssertAuthErrorMessage(String message){
        assertEquals(message, _authPage.authErrorMessage());
    }

}
