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

    @����("������ ILS Plan")
    @When("Click ILS Plan")
    public AuthPage clickILSPlan() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.ILSPlanButton()));
        return _authPage.clickILSPlanButton();
    }

    @����("������ ILS Fact")
    @When("Click ILS Fact")
    public AuthPage clickILSFact() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.ILSFactButton()));
        return _authPage.clickILSFactButton();
    }

    @����("������ ILS Admin")
    @When("Click ILS Admin")
    public AuthPage clickILSAdmin() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.ILSAdminButton()));
        return _authPage.clickILSAdminButton();
    }

    @����("������ ILS Oper")
    @When("Click ILS Oper")
    public AuthPage clickILSOper() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.ILSOperButton()));
        return _authPage.clickILSOperButton();
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
        AssertLogin("");
    }

    @��("����� �� ������")
    @Then("Login is not rmpty")
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
        AssertPassword("");
    }

    @��("������ �� ������")
    @Then("Password is not empty")
    public void AssertPasswordIsNotEmpty(){
        _waiter.until(ExpectedConditions.elementToBeClickable(_authPage.passwordTextField()));
        Assert.assertNotEquals("", _authPage.passwordTextField().getText());
    }

    @��("��������� �������� = (.*)")
    @Then("Pagetitle = (.*)")
    public void AssertPageTitle(String ExpectedPageTitle){
        Assert.assertEquals(ExpectedPageTitle, _authPage.pageTitle());
    }

    @��("��������� �������� ������")
    @Then("Pagetitle is empty")
    public void PageTitleIsEmpty() {
        AssertPageTitle("");
    }

    @��("��������� �������� �� ������")
    @Then("Pagetitle is not empty")
    public void PageTitleIsNotEmpty() {
        Assert.assertNotEquals("", _authPage.pageTitle());
    }

    @��("�������� ��������� (.*)")
    @Then("Message (.*) was showed")
    public void AssertAuthErrorMessage(String message){
        Assert.assertEquals(message, _authPage.authErrorMessage());
    }

    @��("��� ������������ = (.*)")
    @Then("Username = (.*)")
    public void AssertActiveUserName(String username){
        Assert.assertEquals(username, _authPage.menuUser().getText());
    }

    @��("������� ��� ILS Plan = (.*)")
    @Then("Label for ILS Plan = (.*)")
    public void AssertILSPlanLabel(String labelText) {
        Assert.assertEquals(labelText, _authPage.ILSPlanLabel());
    }

    @��("������� ��� ILS Fact = (.*)")
    @Then("Label for ILS Fact = (.*)")
    public void AssertILSFactLabel(String labelText) {
        Assert.assertEquals(labelText, _authPage.ILSFactLabel());
    }

    @��("������� ��� ILS Admin = (.*)")
    @Then("Label for ILS Admin = (.*)")
    public void AssertILSAdminLabel(String labelText) {
        Assert.assertEquals(labelText, _authPage.ILSAdminLabel());
    }

    @��("������� ��� ILS Oper = (.*)")
    @Then("Label for ILS Oper = (.*)")
    public void AssertILSOperLabel(String labelText) {
        Assert.assertEquals(labelText, _authPage.ILSOperLabel());
    }

    @��("�������� ���� (.*)")
    @Then("Active language (.*)")
    public void AssertActiveLanguage(String language) {
        Assert.assertEquals(language, _authPage.getLanguage());
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
