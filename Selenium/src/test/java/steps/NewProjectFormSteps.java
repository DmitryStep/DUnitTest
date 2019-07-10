package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import pageobjects.forms.NewProjectForm;

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

    @����("������ ��� ������� (.*)")
    @When("Type project's name (.*)")
    public void TypeNameOfProject(String ProjectName) {
        _newProjectForm.typeToProjectNameField(ProjectName);
    }

    @����("������ ������� ������")
    @When("Click Create project")
    public void ClickCreate() {
        _newProjectForm.clickCreate();
    }

    @����("������� ������ (.*)")
    @When("Create project (.*)")
    public void CreateNewProject(String ProjectName) {
        TypeNameOfProject(ProjectName);
        ClickCreate();
    }

    @����("�������� �������� �������")
    @When("Cancel creation of project")
    public void CancelCreationOfProject() {
        _newProjectForm.clickCancel();
    }



}
