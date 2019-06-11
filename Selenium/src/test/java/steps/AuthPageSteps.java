package steps;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.Дано;
import cucumber.api.java.ru.Если;
import cucumber.api.java.ru.То;
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

    @Дано("Пользователь авторизован с логином (.*) и паролем (.*)")
    @Given("User authorized with login (.*) and password (.*)")
    @Если("^Авторизоваться с логином (.*) и паролем (.*)$")
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

    @То("Логин = (.*)")
    @Then("Login = (.*)")
    public void AssertLogin(String ExpectedLogin){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        Assert.assertEquals(ExpectedLogin, _authPage.loginTextField().getText());
    }

    @То("Логин пустой")
    @Then("Login is empty")
    public void AssertLoginIsEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        Assert.assertEquals("", _authPage.loginTextField().getText());
    }

    @То("Логин не пустой")
    @Then("Login is not empty")
    public void AssertLoginIsNotEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.loginTextField()));
        Assert.assertNotEquals("", _authPage.loginTextField().getText());
    }

    @То("Пароль = (.*)")
    @Then("Password = (.*)")
    public void AssertPassword(String ExpectedPassword){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        Assert.assertEquals(ExpectedPassword, _authPage.passwordTextField().getText());
    }

    @То("Пароль пустой")
    @Then("Password is empty")
    public void AssertPasswordIsEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        Assert.assertEquals("", _authPage.passwordTextField().getText());
    }

    @То("Пароль не пустой")
    @Then("Password is not empty")
    public void AssertPasswordIsNotEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        Assert.assertNotEquals("", _authPage.passwordTextField().getText());
    }

    @То("Выведено сообщение (.*)")
    @Then("Message (.*) was showed")
    public void AssertAuthErrorMessage(String message){
        Assert.assertEquals(message, _authPage.authErrorMessage());
    }

    @То("Подпись для поля Логин (.*)")
    @Then("Login label")
    public void AssertLoginLabel(String label) {
        Assert.assertEquals(label, _authPage.loginLabel());
    }

    @То("Подпись для поля Пароль (.*)")
    @Then("Password label")
    public void AssertPasswordLabel(String label) {
        Assert.assertEquals(label, _authPage.passwordLabel());
    }

    @То("Подпись на кнопке Авторизации (.*)")
    @Then("Auth button label")
    public void AssertAuthLabel(String label) {
        Assert.assertEquals(label, _authPage.authButtonLabel());
    }

}
