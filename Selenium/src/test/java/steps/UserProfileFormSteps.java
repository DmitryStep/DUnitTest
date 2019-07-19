package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.��;
import pageobjects.forms.UserProfileForm;

public class UserProfileFormSteps extends BaseSteps {

    private UserProfileForm _userProfileForm = null;

    //----------------------------------------- Hooks -----------------------------------------------------

    @Before(order = 4)
    public void beforeTest() {
        _userProfileForm = new UserProfileForm(_driver, _waiter);
    }

    @After(order = 4)
    public void afterTest() {
        AfterTest();
        _userProfileForm = null;
    }

    //----------------------------------------- Steps -----------------------------------------------------

    @����("������ ������� (.*)")
    @When("Type account (.*)")
    public void TypeAccount(String username) {
        _userProfileForm.typeToAccountField(username);
    }

    @����("������ ������ ������ (.*)")
    @Given("Type old password (.*)")
    public void TypeOldPassword(String oldPassword) {
        _userProfileForm.typeOldPassword(oldPassword);
    }

    @����("������ ����� ������ (.*)")
    @Given("Type new password (.*)")
    public void TypeNewPassword(String newPassword) {
        _userProfileForm.typeNewPassword(newPassword);
    }

    @����("������ ������������� ������ (.*)")
    @Given("Type password confirmation (.*)")
    public void TypePasswordConfirmation(String confirmPassword) {
        _userProfileForm.typeConfirmNewPassword(confirmPassword);
    }

    @����("������ �������� ������")
    @When("Click Change password")
    public void ClickChangePassword() {
        _userProfileForm.changePasswordButtonClick();
    }

    @����("�������� ������ (.*) �� (.*)")
    @When("Change password from (.*) �� (.*)")
    public void ChangePassword(String oldPassword, String newPassword) {
        TypeOldPassword(oldPassword);
        TypeNewPassword(newPassword);
        TypePasswordConfirmation(newPassword);
        ClickChangePassword();
    }

    @����("��������� �������")
    @When("Save account")
    public void SaveAccount() {
        _userProfileForm.saveButtonClick();
    }

    @����("�������� ������� �� (.*)")
    @When("Change account to (.*)")
    public void ChangeAccount(String newAccount) {
        TypeAccount(newAccount);
        SaveAccount();
    }

    @����("�������� �������")
    @When("Clear account")
    public void ChangeAccountToEmptyValue() {
        TypeAccount("");
        SaveAccount();
    }

    @����("������� ����� ������� ������������")
    @When("Close user profile form")
    public void CloseForm() {
        _userProfileForm.closeButtonClick();
    }

    //--------------------------------------- Assertions --------------------------------------------------

    @��("��� ������������ = (.*)")
    @Then("Username = (.*)")
    public void AssertUsername(String username) {
        assertEquals(username, _userProfileForm.userFieldValue());
    }

    @��("������ ������ = (.*)")
    @Then("Old password = (.*)")
    public void AssertOldPassword(String oldPassword) {
        assertEquals(oldPassword, _userProfileForm.oldPasswordValue());
    }

    @��("������ ������ ������")
    @Then("Old password is empty")
    public void AssertOldPasswordIsEmpty() {
        assertEquals("", _userProfileForm.oldPasswordValue());
    }

    @��("������ ������ �� ������")
    @Then("Old password is not empty")
    public void AssertOldPasswordIsNotEmpty() {
        assertNotEquals("", _userProfileForm.oldPasswordValue());
    }

    @��("����� ������ = (.*)")
    @Then("New password = (.*)")
    public void AssertNewPassword(String newPassword) {
        assertEquals(newPassword, _userProfileForm.newPasswordValue());
    }

    @��("����� ������ ������")
    @Then("New password is empty")
    public void AssertNewPasswordIsEmpty() {
        assertEquals("", _userProfileForm.newPasswordValue());
    }

    @��("����� ������ �� ������")
    @Then("New password is not empty")
    public void AssertNewPasswordIsNotEmpty() {
        assertNotEquals("", _userProfileForm.newPasswordValue());
    }

    @��("������������� ������ = (.*)")
    @Then("Password's confirmation = (.*)")
    public void AssertConfirmNewPassword(String confirmPassword) {
        assertEquals(confirmPassword, _userProfileForm.confirmNewPasswordValue());
    }

    @��("������������� ������ ������")
    @Then("Password's confirmation is empty")
    public void AssertConfirmNewPasswordIsEmpty() {
        assertEquals("", _userProfileForm.confirmNewPasswordValue());
    }

    @��("������������� ������ �� ������")
    @Then("Password's confirmation is not empty")
    public void AssertConfirmNewPasswordIsNotEmpty() {
        assertNotEquals("", _userProfileForm.confirmNewPasswordValue());
    }
}
