package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.��;
import org.openqa.selenium.support.ui.ExpectedConditions;
import pageobjects.pages.ProjectsPage;
import java.util.List;

public class ProjectsPageSteps extends BaseSteps {

    private ProjectsPage _projectsPage = null;

    // -------------------------------------------- Hooks ---------------------------------------------------------

    @Before(order = 7)
    public void beforeTest(){
        _projectsPage = new ProjectsPage(_driver);
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
        _waiter.until(ExpectedConditions.elementToBeClickable(_projectsPage.newProjectButton()));
        _projectsPage.newProjectButtonClick();
    }

    @����("������ ���� ������ (.*)")
    @When("Type begin date (.*)")
    public void TypeBeginDate(String BeginDate) {
        _waiter.until(ExpectedConditions.elementToBeClickable(_projectsPage.beginDate()));
        _projectsPage.typeBeginDate(BeginDate);
    }

    @����("������ ���� ��������� (.*)")
    @When("Type end date (.*)")
    public void TypeEndDate(String EndDate) {
        _waiter.until(ExpectedConditions.elementToBeClickable(_projectsPage.beginDate()));
        _projectsPage.typeEndDate(EndDate);
    }

    @����("������ ��������")
    @When("Click Show")
    public void FilterResultsClick() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_projectsPage.showFilterResults()));
        _projectsPage.showFilterResultsClick();
    }

    @����("�������� ������� � (.*) �� (.*)")
    @When("Show projects from (.*) to (.*)")
    public void ShowProjectsByPeriod(String BeginDate, String EndDate) {
        TypeBeginDate(BeginDate);
        TypeEndDate(EndDate);
        FilterResultsClick();
    }

    @����("�������� ����� ������")
    @When("Add new project")
    public void AddNewProject() {
        _projectsPage.newProjectButtonClick();
    }

    @����("����������� ������ � ID = (.*)")
    @When("Double project with ID = (.*)")
    public void DoubleProjectByID(String ID){
        _projectsPage.doubleProjectClick(_projectsPage.getTableStringByProjectID(ID));
    }

    @����("����������� ������ �� ������ (.*)")
    @When("Double project from string (.*)")
    public void DoubleProjectByTabString(int tabString){
        _projectsPage.doubleProjectClick(_projectsPage.getTableStringByNumber(tabString));
    }

    @����("������������� ������ � ID = (.*)")
    @When("Edit project with ID = (.*)")
    public void EditProjectByID(String ID) {
        _projectsPage.editProjectClick(_projectsPage.getTableStringByProjectID(ID));
    }

    @����("������������� ������ (.*)")
    @When("Edit project (.*)")
    public void EditProjectByProjectName(String projectName) {
        _projectsPage.editProjectClick(_projectsPage.getTableStringByProjectName(projectName));
    }

    @����("������������� ������ �� ������ (.*)")
    @When("Edit project from string (.*)")
    public void EditProjectByTabString(int tabString){
        _projectsPage.editProjectClick(_projectsPage.getTableStringByNumber(tabString));
    }

    @����("������� ������ � ID = (.*)")
    @When("Delete project with ID = (.*)")
    public void DeleteProjectByID(String ID) {
        _projectsPage.deleteProjectClick(_projectsPage.getTableStringByProjectID(ID));
    }

    @����("������� ������ (.*)")
    @When("Delete project (.*)")
    public void DeleteProjectByProjectName(String projectName) {
        _projectsPage.deleteProjectClick(_projectsPage.getTableStringByProjectName(projectName));
    }

    @����("������� ������ ������� ����� (.*)")
    @When("Delete table string number (.*)")
    public void DeleteProjectByStringNumber(int strNumber) {
        _projectsPage.deleteProjectClick(_projectsPage.getTableStringByNumber(strNumber));
    }

    @����("������� ������ ������� � (.*) �� (.*)")
    @When("Delete table strings from (.*) to (.*)")
    public void DeleteProjectsByStringNumbers(int strNumberBegin, int strNumberEnd) {
        for (int i = strNumberEnd; i >= strNumberBegin; i--) {
            DeleteProjectByStringNumber(i);
        }
    }

    @����("������� ������� � ID (.*)")
    @When("Delete projects with IDs (.*)")
    public void DeleteProjectsListByIDs(List<String> IDs) {
        for (String projectID: IDs) {
            DeleteProjectByID(projectID);
        }
    }

    // ---------------------------------------- Assertions --------------------------------------------------------

    @��("���������� �������� � ������� = (.*)")
    @Then("Count of projects = (.*)")
    public void AssertCountOfProjects(int Count) {
        assertEquals(Count, _projectsPage.getTableStringsCount());
    }

}
