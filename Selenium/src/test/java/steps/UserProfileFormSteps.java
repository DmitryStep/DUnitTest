package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.Если;
import cucumber.api.java.ru.То;
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

    @Если("Ввести аккаунт (.*)")
    @When("Type account (.*)")
    public void TypeAccount(String username) {
        _userProfileForm.typeToAccountField(username);
    }

    @Если("Ввести старый пароль (.*)")
    @Given("Type old password (.*)")
    public void TypeOldPassword(String oldPassword) {
        _userProfileForm.typeOldPassword(oldPassword);
    }

    @Если("Ввести новый пароль (.*)")
    @Given("Type new password (.*)")
    public void TypeNewPassword(String newPassword) {
        _userProfileForm.typeNewPassword(newPassword);
    }

    @Если("Ввести подтверждение пароля (.*)")
    @Given("Type password confirmation (.*)")
    public void TypePasswordConfirmation(String confirmPassword) {
        _userProfileForm.typeConfirmNewPassword(confirmPassword);
    }

    @Если("Нажать Изменить пароль")
    @When("Click Change password")
    public void ClickChangePassword() {
        _userProfileForm.changePasswordButtonClick();
    }

    @Если("Изменить пароль (.*) на (.*)")
    @When("Change password from (.*) на (.*)")
    public void ChangePassword(String oldPassword, String newPassword) {
        TypeOldPassword(oldPassword);
        TypeNewPassword(newPassword);
        TypePasswordConfirmation(newPassword);
        ClickChangePassword();
    }

    @Если("Сохранить аккаунт")
    @When("Save account")
    public void SaveAccount() {
        _userProfileForm.saveButtonClick();
    }

    @Если("Изменить аккаунт на (.*)")
    @When("Change account to (.*)")
    public void ChangeAccount(String newAccount) {
        TypeAccount(newAccount);
        SaveAccount();
    }

    @Если("Очистить аккаунт")
    @When("Clear account")
    public void ChangeAccountToEmptyValue() {
        TypeAccount("");
        SaveAccount();
    }

    @Если("Закрыть форму профиля пользователя")
    @When("Close user profile form")
    public void CloseForm() {
        _userProfileForm.closeButtonClick();
    }

    //--------------------------------------- Assertions --------------------------------------------------

    @То("Имя пользователя = (.*)")
    @Then("Username = (.*)")
    public void AssertUsername(String username) {
        assertEquals(username, _userProfileForm.userFieldValue());
    }

    @То("Старый пароль = (.*)")
    @Then("Old password = (.*)")
    public void AssertOldPassword(String oldPassword) {
        assertEquals(oldPassword, _userProfileForm.oldPasswordValue());
    }

    @То("Старый пароль пустой")
    @Then("Old password is empty")
    public void AssertOldPasswordIsEmpty() {
        assertEquals("", _userProfileForm.oldPasswordValue());
    }

    @То("Старый пароль не пустой")
    @Then("Old password is not empty")
    public void AssertOldPasswordIsNotEmpty() {
        assertNotEquals("", _userProfileForm.oldPasswordValue());
    }

    @То("Новый пароль = (.*)")
    @Then("New password = (.*)")
    public void AssertNewPassword(String newPassword) {
        assertEquals(newPassword, _userProfileForm.newPasswordValue());
    }

    @То("Новый пароль пустой")
    @Then("New password is empty")
    public void AssertNewPasswordIsEmpty() {
        assertEquals("", _userProfileForm.newPasswordValue());
    }

    @То("Новый пароль не пустой")
    @Then("New password is not empty")
    public void AssertNewPasswordIsNotEmpty() {
        assertNotEquals("", _userProfileForm.newPasswordValue());
    }

    @То("Подтверждение пароля = (.*)")
    @Then("Password's confirmation = (.*)")
    public void AssertConfirmNewPassword(String confirmPassword) {
        assertEquals(confirmPassword, _userProfileForm.confirmNewPasswordValue());
    }

    @То("Подтверждение пароля пустое")
    @Then("Password's confirmation is empty")
    public void AssertConfirmNewPasswordIsEmpty() {
        assertEquals("", _userProfileForm.confirmNewPasswordValue());
    }

    @То("Подтверждение пароля не пустое")
    @Then("Password's confirmation is not empty")
    public void AssertConfirmNewPasswordIsNotEmpty() {
        assertNotEquals("", _userProfileForm.confirmNewPasswordValue());
    }
}
