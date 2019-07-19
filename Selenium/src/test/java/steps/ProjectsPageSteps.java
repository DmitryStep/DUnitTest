package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.��;
import pageobjects.pages.ProjectsPage;


public class ProjectsPageSteps extends BaseSteps {

    private ProjectsPage _projectsPage = null;

    // -------------------------------------------- Hooks ---------------------------------------------------------

    @Before(order = 7)
    public void beforeTest(){
        _projectsPage = new ProjectsPage(_driver, _waiter);
    }

    @After(order = 7)
    public void afterTest() {
        AfterTest();
        _projectsPage = null;
    }

    // --------------------------------------------------- Steps --------------------------------------------------

    @����("������ ������� ����� ������")
    @When("Click Create new project")
    public void CreateNewProjectClick() {
//        _waiter.until(ExpectedConditions.elementToBeClickable(_projectsPage.newProjectButton()));
//        _projectsPage.newProjectButtonClick();
    }

    @����("�������� ������� �� ������ (.*) - (.*)")
    @When("Show projects for period (.*) - (.*)")
    public void TypeBeginDate(String DateBegin, String DateEnd) {
        _projectsPage.ResetGrouping();
        _projectsPage.setProjectsPeriod(DateBegin, DateEnd);
    }


    // ---------------------------------------- Assertions --------------------------------------------------------

    @��("���������� �������� � ������� = (.*)")
    @Then("Count of projects = (.*)")
    public void AssertCountOfProjects(int Count) {
        assertEquals(Count, _projectsPage.getTableStringsCount());
    }

}
