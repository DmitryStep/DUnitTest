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

    @����("������������ ����������� � ������� (.*) � ������� (.*)")
    @Given("User authorized with login (.*) and password (.*)")
    @����("^�������������� � ������� (.*) � ������� (.*)$")
    @When("^Authorize with login (.*) and password (.*)$")
    public AuthPage Authorize(String login, String password){
        _authPage.typeLogin(login);
        _authPage.typePassword(password);
        return _authPage.clickAuthButton();
    }

    @����("������ �� �������")
    @When("Click logo")
    public AuthPage clickLogo() {
        return _authPage.clickLogo();
    }

    @����("������ ILS Plan")
    @When("Click ILS Plan")
    public AuthPage clickILSPlan() {
        return _authPage.clickILSPlanButton();
    }

    @����("������ ILS Fact")
    @When("Click ILS Fact")
    public AuthPage clickILSFact() {
        return _authPage.clickILSFactButton();
    }

    @����("������ ILS Admin")
    @When("Click ILS Admin")
    public AuthPage clickILSAdmin() {
        return _authPage.clickILSAdminButton();
    }


    @����("������ ILS Oper")
    @When("Click ILS Oper")
    public AuthPage clickILSOper() {
        return _authPage.clickILSOperButton();
    }

    @����("�������� �� ������")
    @When("Click version")
    public AuthPage clickVersion() {
        return _authPage.clickVersion();
    }

    @����("������ ���� ������������")
    @When("Click usermenu")
    public void clickUser() {
        _authPage.clickUserMenu();
    }

    @����("������ �����")
    @When("Click Exit")
    public AuthPage clickExit() {
        return _authPage.clickMenuUserExit();
    }

    @����("������� ���� (.*)")
    @When("Select language (.*)")
    public AuthPage selectLanguage(String language) {
        return _authPage.selectLanguage(language);
    }

    //--------------------------------------- Assertions --------------------------------------------------

    @��("����� = (.*)")
    @Then("Login = (.*)")
    public void AssertLogin(String ExpectedLogin){
        Assert.assertEquals(ExpectedLogin, _authPage.loginTextField().getText());
    }

    @��("����� ������")
    @Then("Login is empty")
    public void AssertLoginIsEmpty(){
        AssertLogin("");
    }

    @��("����� �� ������")
    @Then("Login is not rmpty")
    public void AssertLoginIsNotEmpty(){
        Assert.assertNotEquals("", _authPage.loginTextField().getText());
    }

    @��("������ = (.*)")
    @Then("Password = (.*)")
    public void AssertPassword(String ExpectedPassword){
        Assert.assertEquals(ExpectedPassword, _authPage.passwordTextField().getText());
    }

    @��("������ ������")
    @Then("Password is empty")
    public void AssertPasswordIsEmpty(){
        AssertPassword("");
    }

    @��("������ �� ������")
    @Then("Password is not empty")
    public void AssertPasswordIsNotEmpty(){
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
