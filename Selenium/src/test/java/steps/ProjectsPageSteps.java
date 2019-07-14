package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.Если;
import cucumber.api.java.ru.То;
import org.openqa.selenium.support.ui.ExpectedConditions;
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

    @Если("Нажать Создать новый проект")
    @When("Click Create new project")
    public void CreateNewProjectClick() {
//        _waiter.until(ExpectedConditions.elementToBeClickable(_projectsPage.newProjectButton()));
//        _projectsPage.newProjectButtonClick();
    }

    @Если("Показать проекты за период (.*) - (.*)")
    @When("Show projects for period (.*) - (.*)")
    public void TypeBeginDate(String DateBegin, String DateEnd) {
        _projectsPage.ResetGrouping();
        _projectsPage.setProjectsPeriod(DateBegin, DateEnd);
    }


    // ---------------------------------------- Assertions --------------------------------------------------------

    @То("Количество проектов в таблице = (.*)")
    @Then("Count of projects = (.*)")
    public void AssertCountOfProjects(int Count) {
        int tsCount;
        try {
            _waiter.until(ExpectedConditions.elementToBeClickable(_projectsPage.projectTable()));
            tsCount = _projectsPage.getTableStringsCount();
        } catch (Exception e) {
            tsCount = 0;
        }
        assertEquals(Count, tsCount);
    }

}
