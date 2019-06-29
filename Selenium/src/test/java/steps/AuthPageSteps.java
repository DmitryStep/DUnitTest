package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.Дано;
import cucumber.api.java.ru.Если;
import cucumber.api.java.ru.То;
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

    @Если("Ввести логин (.*)")
    @When("Type login (.*)")
    public void TypeLogin(String login) {
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        _authPage.typeLogin(login);
    }

    @Если("Ввести пароль (.*)")
    @When("Type password (.*)")
    public void TypePassword(String password) {
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        _authPage.typePassword(password);
    }

    @Если("Нажать Войти в систему")
    @When("Click Login to the system")
    public void ClickAuthButton() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.authButton()));
        _authPage.clickAuthButton();
    }

    @Дано("Пользователь авторизован с логином (.*) и паролем (.*)")
    @Given("User authorized with login (.*) and password (.*)")
    @Если("^Авторизоваться с логином (.*) и паролем (.*)$")
    @When("^Authorize with login (.*) and password (.*)$")
    public void Authorize(String login, String password){
        TypeLogin(login);
        TypePassword(password);
        ClickAuthButton();
    }

    //--------------------------------------- Assertions --------------------------------------------------

    @То("Логин = (.*)")
    @Then("Login = (.*)")
    public void AssertLogin(String ExpectedLogin){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        assertEquals(ExpectedLogin, _authPage.loginTextField().getText());
    }

    @То("Логин пустой")
    @Then("Login is empty")
    public void AssertLoginIsEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        assertEquals("", _authPage.loginTextField().getText());
    }

    @То("Логин не пустой")
    @Then("Login is not empty")
    public void AssertLoginIsNotEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        assertNotEquals("", _authPage.loginTextField().getText());
    }

    @То("Пароль = (.*)")
    @Then("Password = (.*)")
    public void AssertPassword(String ExpectedPassword){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        assertEquals(ExpectedPassword, _authPage.passwordTextField().getText());
    }

    @То("Пароль пустой")
    @Then("Password is empty")
    public void AssertPasswordIsEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        assertEquals("", _authPage.passwordTextField().getText());
    }

    @То("Пароль не пустой")
    @Then("Password is not empty")
    public void AssertPasswordIsNotEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        assertNotEquals("", _authPage.passwordTextField().getText());
    }

    @То("Выведено сообщение (.*)")
    @Then("Message (.*) was showed")
    public void AssertAuthErrorMessage(String message){
        assertEquals(message, _authPage.authErrorMessage());
    }

}
