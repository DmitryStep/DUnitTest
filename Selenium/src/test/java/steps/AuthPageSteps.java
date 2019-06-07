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
import pages.AuthPage;
import utils.WebDriverManager;

public class AuthPageSteps extends WebDriverManager {

    private AuthPage _authPage = null;

    @Before
    public void beforeTest(){
        _authPage = new AuthPage(_driver);
    }

    @After
    public void afterTest() {
        _driver.manage().deleteAllCookies();
        _authPage = null;
    }

    //----------------------------------------- Steps -----------------------------------------------------

    @Дано("Пользователь авторизован с логином (.*) и паролем (.*)")
    @Given("User authorized with login (.*) and password (.*)")
    @Если("^Авторизоваться с логином (.*) и паролем (.*)$")
    @When("^Authorize with login (.*) and password (.*)$")
    public AuthPage Authorize(String login, String password){
        _authPage.typeLogin(login);
        _authPage.typePassword(password);
        return _authPage.clickAuthButton();
    }

    @Если("Нажать на логотип")
    @When("Click logo")
    public AuthPage clickLogo() {
        return _authPage.clickLogo();
    }

    @Если("Нажать ILS Plan")
    @When("Click ILS Plan")
    public AuthPage clickILSPlan() {
        return _authPage.clickILSPlanButton();
    }

    @Если("Нажать ILS Fact")
    @When("Click ILS Fact")
    public AuthPage clickILSFact() {
        return _authPage.clickILSFactButton();
    }

    @Если("Нажать ILS Admin")
    @When("Click ILS Admin")
    public AuthPage clickILSAdmin() {
        return _authPage.clickILSAdminButton();
    }


    @Если("Нажать ILS Oper")
    @When("Click ILS Oper")
    public AuthPage clickILSOper() {
        return _authPage.clickILSOperButton();
    }

    @Если("Кликнуть по версии")
    @When("Click version")
    public AuthPage clickVersion() {
        return _authPage.clickVersion();
    }

    @Если("Нажать меню пользователя")
    @When("Click usermenu")
    public void clickUser() {
        _authPage.clickUserMenu();
    }

    @Если("Нажать Выход")
    @When("Click Exit")
    public AuthPage clickExit() {
        return _authPage.clickMenuUserExit();
    }

    @Если("Выбрать язык (.*)")
    @When("Select language (.*)")
    public AuthPage selectLanguage(String language) {
        return _authPage.selectLanguage(language);
    }

    //--------------------------------------- Assertions --------------------------------------------------

    @То("Логин = (.*)")
    @Then("Login = (.*)")
    public void AssertLogin(String ExpectedLogin){
        Assert.assertEquals(ExpectedLogin, _authPage.loginTextField().getText());
    }

    @То("Логин пустой")
    @Then("Login is empty")
    public void AssertLoginIsEmpty(){
        AssertLogin("");
    }

    @То("Логин не пустой")
    @Then("Login is not rmpty")
    public void AssertLoginIsNotEmpty(){
        Assert.assertNotEquals("", _authPage.loginTextField().getText());
    }

    @То("Пароль = (.*)")
    @Then("Password = (.*)")
    public void AssertPassword(String ExpectedPassword){
        Assert.assertEquals(ExpectedPassword, _authPage.passwordTextField().getText());
    }

    @То("Пароль пустой")
    @Then("Password is empty")
    public void AssertPasswordIsEmpty(){
        AssertPassword("");
    }

    @То("Пароль не пустой")
    @Then("Password is not empty")
    public void AssertPasswordIsNotEmpty(){
        Assert.assertNotEquals("", _authPage.passwordTextField().getText());
    }

    @То("Заголовок страницы = (.*)")
    @Then("Pagetitle = (.*)")
    public void AssertPageTitle(String ExpectedPageTitle){
        Assert.assertEquals(ExpectedPageTitle, _authPage.pageTitle());
    }

    @То("Заголовок страницы пустой")
    @Then("Pagetitle is empty")
    public void PageTitleIsEmpty() {
        AssertPageTitle("");
    }

    @То("Заголовок страницы не пустой")
    @Then("Pagetitle is not empty")
    public void PageTitleIsNotEmpty() {
        Assert.assertNotEquals("", _authPage.pageTitle());
    }

    @То("Выведено сообщение (.*)")
    @Then("Message (.*) was showed")
    public void AssertAuthErrorMessage(String message){
        Assert.assertEquals(message, _authPage.authErrorMessage());
    }

    @То("Имя пользователя = (.*)")
    @Then("Username = (.*)")
    public void AssertActiveUserName(String username){
        Assert.assertEquals(username, _authPage.menuUser().getText());
    }

    @То("Подпись для ILS Plan = (.*)")
    @Then("Label for ILS Plan = (.*)")
    public void AssertILSPlanLabel(String labelText) {
        Assert.assertEquals(labelText, _authPage.ILSPlanLabel());
    }

    @То("Подпись для ILS Fact = (.*)")
    @Then("Label for ILS Fact = (.*)")
    public void AssertILSFactLabel(String labelText) {
        Assert.assertEquals(labelText, _authPage.ILSFactLabel());
    }

    @То("Подпись для ILS Admin = (.*)")
    @Then("Label for ILS Admin = (.*)")
    public void AssertILSAdminLabel(String labelText) {
        Assert.assertEquals(labelText, _authPage.ILSAdminLabel());
    }

    @То("Подпись для ILS Oper = (.*)")
    @Then("Label for ILS Oper = (.*)")
    public void AssertILSOperLabel(String labelText) {
        Assert.assertEquals(labelText, _authPage.ILSOperLabel());
    }

    @То("Активный язык (.*)")
    @Then("Active language (.*)")
    public void AssertActiveLanguage(String language) {
        Assert.assertEquals(language, _authPage.getLanguage());
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
