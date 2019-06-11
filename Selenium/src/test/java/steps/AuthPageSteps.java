package steps;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.��;
import org.junit.Assert;
import org.openqa.selenium.support.ui.ExpectedConditions;
import pages.AuthPage;
import utils.WebDriverManager;

public class AuthPageSteps extends WebDriverManager {

    private AuthPage _authPage = null;

    //----------------------------------------- Hooks -----------------------------------------------------

    @Before
    public void beforeTest(){
        _authPage = new AuthPage(_driver);
    }

    @After
    public void afterTest() {
        deleteCookies();
        refreshPage();
        _authPage = null;
    }

    //----------------------------------------- Steps -----------------------------------------------------

    @����("������������ ����������� � ������� (.*) � ������� (.*)")
    @Given("User authorized with login (.*) and password (.*)")
    @����("^�������������� � ������� (.*) � ������� (.*)$")
    @When("^Authorize with login (.*) and password (.*)$")
    public AuthPage Authorize(String login, String password){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        _authPage.typeLogin(login);
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        _authPage.typePassword(password);
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.authButton()));
        return _authPage.clickAuthButton();
    }

    //--------------------------------------- Assertions --------------------------------------------------

    @��("����� = (.*)")
    @Then("Login = (.*)")
    public void AssertLogin(String ExpectedLogin){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        Assert.assertEquals(ExpectedLogin, _authPage.loginTextField().getText());
    }

    @��("����� ������")
    @Then("Login is empty")
    public void AssertLoginIsEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        Assert.assertEquals("", _authPage.loginTextField().getText());
    }

    @��("����� �� ������")
    @Then("Login is not empty")
    public void AssertLoginIsNotEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        Assert.assertNotEquals("", _authPage.loginTextField().getText());
    }

    @��("������ = (.*)")
    @Then("Password = (.*)")
    public void AssertPassword(String ExpectedPassword){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        Assert.assertEquals(ExpectedPassword, _authPage.passwordTextField().getText());
    }

    @��("������ ������")
    @Then("Password is empty")
    public void AssertPasswordIsEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        Assert.assertEquals("", _authPage.passwordTextField().getText());
    }

    @��("������ �� ������")
    @Then("Password is not empty")
    public void AssertPasswordIsNotEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        Assert.assertNotEquals("", _authPage.passwordTextField().getText());
    }

    @��("�������� ��������� (.*)")
    @Then("Message (.*) was showed")
    public void AssertAuthErrorMessage(String message){
        Assert.assertEquals(message, _authPage.authErrorMessage());
    }

    @��("������� ��� ���� ����� (.*)")
    @Then("Login label")
    public void AssertLoginLabel(String label) {
        Assert.assertEquals(label, _authPage.loginLabel());
    }

    @��("������� ��� ���� ������ (.*)")
    @Then("Password label")
    public void AssertPasswordLabel(String label) {
        Assert.assertEquals(label, _authPage.passwordLabel());
    }

    @��("������� �� ������ ����������� (.*)")
    @Then("Auth button label")
    public void AssertAuthLabel(String label) {
        Assert.assertEquals(label, _authPage.authButtonLabel());
    }

}
