package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Then;
import cucumber.api.java.ru.То;
import pageobjects.forms.MessageForm;

public class MessageFormSteps extends BaseSteps {

    MessageForm _MessageForm = null;

    @Before(order = 5)
    public void beforeTest(){
        _MessageForm = new MessageForm(_driver, _waiter);
    }

    @After(order = 5)
    public void afterTest() {
        AfterTest();
        _MessageForm = null;
    }


    @То("Ошибка (.*)")
    @Then("Error (.*)")
    public void assertErrorMessage(String ExpectedMessage) {
        assertEquals(ExpectedMessage, _MessageForm.getMessage());
    }
}
