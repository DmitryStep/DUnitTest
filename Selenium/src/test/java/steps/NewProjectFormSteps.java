package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.Если;
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

    @Если("Ввести имя проекта (.*)")
    @When("Type project's name (.*)")
    public void TypeNameOfProject(String ProjectName) {
        _newProjectForm.typeToProjectNameField(ProjectName);
    }

    @Если("Нажать Создать проект")
    @When("Click Create project")
    public void ClickCreate() {
        _newProjectForm.clickCreate();
    }

    @Если("Создать проект (.*)")
    @When("Create project (.*)")
    public void CreateNewProject(String ProjectName) {
        TypeNameOfProject(ProjectName);
        ClickCreate();
    }

    @Если("Отменить создание проекта")
    @When("Cancel creation of project")
    public void CancelCreationOfProject() {
        _newProjectForm.clickCancel();
    }



}
