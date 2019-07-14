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

    @Если("Ввести логин (.*)")
    @When("Type login (.*)")
    public void TypeLogin(String login) {
        _authPage.typeLogin(login);
    }

    @Если("Ввести пароль (.*)")
    @When("Type password (.*)")
    public void TypePassword(String password) {
        _authPage.typePassword(password);
    }

    @Если("Нажать Войти в систему")
    @When("Click Login to the system")
    public void ClickAuthButton() {
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


    @Дано("Пользователь авторизован в системе")
    @Given("User authorized in the system")
    @Если("Авторизоваться в системе")
    @When("Authorize in the system")
    public void baseAuthorization() {
        getBaseUrl();
        Authorize(_baseLogin, _basePassword);
    }
    //--------------------------------------- Assertions --------------------------------------------------

    @То("Логин = (.*)")
    @Then("Login = (.*)")
    public void AssertLogin(String ExpectedLogin){
        assertEquals(ExpectedLogin, _authPage.getLoginValue());
    }

    @То("Логин пустой")
    @Then("Login is empty")
    public void AssertLoginIsEmpty(){
        assertEquals("", _authPage.getLoginValue());
    }

    @То("Логин не пустой")
    @Then("Login is not empty")
    public void AssertLoginIsNotEmpty(){
        assertNotEquals("", _authPage.getLoginValue());
    }

    @То("Пароль = (.*)")
    @Then("Password = (.*)")
    public void AssertPassword(String ExpectedPassword){
        assertEquals(ExpectedPassword, _authPage.getPasswordValue());
    }

    @То("Пароль пустой")
    @Then("Password is empty")
    public void AssertPasswordIsEmpty(){
        assertEquals("", _authPage.getPasswordValue());
    }

    @То("Пароль не пустой")
    @Then("Password is not empty")
    public void AssertPasswordIsNotEmpty(){
        assertNotEquals("", _authPage.getPasswordValue());
    }

    @То("Выведено сообщение (.*)")
    @Then("Message (.*) was showed")
    public void AssertAuthErrorMessage(String message){
        assertEquals(message, _authPage.authErrorMessage());
    }

}
