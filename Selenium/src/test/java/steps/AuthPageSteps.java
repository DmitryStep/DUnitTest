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
import pageobjects.pages.AuthPage;

public class AuthPageSteps extends BaseSteps {

    private AuthPage _authPage = null;

    //----------------------------------------- Hooks -----------------------------------------------------

    @Before(order=1)
    public void beforeTest(){
        _authPage = new AuthPage(_driver, _waiter);
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
        _authPage.typeLogin(login);
    }

    @����("������ ������ (.*)")
    @When("Type password (.*)")
    public void TypePassword(String password) {
        _authPage.typePassword(password);
    }

    @����("������ ����� � �������")
    @When("Click Login to the system")
    public void ClickAuthButton() {
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


    @����("������������ ����������� � �������")
    @Given("User authorized in the system")
    @����("�������������� � �������")
    @When("Authorize in the system")
    public void baseAuthorization() {
        getBaseUrl();
        Authorize(_baseLogin, _basePassword);
    }
    //--------------------------------------- Assertions --------------------------------------------------

    @��("����� = (.*)")
    @Then("Login = (.*)")
    public void AssertLogin(String ExpectedLogin){
        assertEquals(ExpectedLogin, _authPage.getLoginValue());
    }

    @��("����� ������")
    @Then("Login is empty")
    public void AssertLoginIsEmpty(){
        assertEquals("", _authPage.getLoginValue());
    }

    @��("����� �� ������")
    @Then("Login is not empty")
    public void AssertLoginIsNotEmpty(){
        assertNotEquals("", _authPage.getLoginValue());
    }

    @��("������ = (.*)")
    @Then("Password = (.*)")
    public void AssertPassword(String ExpectedPassword){
        assertEquals(ExpectedPassword, _authPage.getPasswordValue());
    }

    @��("������ ������")
    @Then("Password is empty")
    public void AssertPasswordIsEmpty(){
        assertEquals("", _authPage.getPasswordValue());
    }

    @��("������ �� ������")
    @Then("Password is not empty")
    public void AssertPasswordIsNotEmpty(){
        assertNotEquals("", _authPage.getPasswordValue());
    }

    @��("�������� ��������� (.*)")
    @Then("Message (.*) was showed")
    public void AssertAuthErrorMessage(String message){
        assertEquals(message, _authPage.authErrorMessage());
    }

}
