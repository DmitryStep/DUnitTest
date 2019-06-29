package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import forms.NewProjectForm;
import pages.HeaderPage;

public class NewProjectFormSteps extends BaseSteps {

    private NewProjectForm _newProjectForm = null;

    @Before(order = 6)
    public void beforeTest(){
        _newProjectForm = new NewProjectForm(_driver);
    }

    @After(order = 6)
    public void afterTest() {
        AfterTest();
        _newProjectForm = null;
    }



}
