package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Then;
import cucumber.api.java.ru.То;
import forms.ErrorMessageForm;

public class ErrorMessageFormSteps extends BaseSteps {

    ErrorMessageForm _errorMessageForm = null;

    @Before(order = 5)
    public void beforeTest(){
        _errorMessageForm = new ErrorMessageForm(_driver);
    }

    @After(order = 5)
    public void afterTest() {
        AfterTest();
        _errorMessageForm = null;
    }


    @То("Ошибка (.*)")
    @Then("Error (.*)")
    public void assertErrorMessage(String ExpectedMessage) {
        assertEquals(ExpectedMessage, _errorMessageForm.getErrorMessage());
    }
}
