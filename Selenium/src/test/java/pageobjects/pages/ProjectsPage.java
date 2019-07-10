package pageobjects.pages;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.util.List;

public class ProjectsPage extends BasePage {

    public ProjectsPage(WebDriver driver){
        super(driver);
    }

    // ---------------------------------- ProjectsPage WebElements ---------------------------------------------

    // ������ "������� ����� ������"
    public WebElement newProjectButton(){
        return _driver.findElement(By.xpath(".//*/div[@class=\"ils-log-welcome\"]/*/button[@class=\"ils-button\"]"));
    }

    // ������ ������ "������� ����� ������"
    public void newProjectButtonClick() {
        newProjectButton().click();
    }

    // ���� ������ ������� �������
    public WebElement beginDate() {
        return _driver.findElement(By.xpath(".//*[@id=\"project_date1\"]"));
    }

    // ������ ���� ������ �������
    public void typeBeginDate(String BeginDate) {
        beginDate().clear();
        beginDate().sendKeys(BeginDate);
    }

    // ���� ��������� ������� �������
    public WebElement endDate() {
        return _driver.findElement(By.xpath(".//*[@id=\"project_date2\"]"));
    }

    // ������ ���� ��������� �������
    public void typeEndDate(String EndDate) {
        endDate().clear();
        endDate().sendKeys(EndDate);
    }

    // ������ "��������"
    public WebElement showFilterResults() {
        return _driver.findElement(By.xpath(".//*/div[@class=\"ils-log-welcome-form\"]/*/button[@class=\"ils-button\"]"));
    }

    // ���� �� ������ "��������"
    public void showFilterResultsClick() {
        showFilterResults().click();
    }

    // ��������� ������� ��������
    public WebElement projectsTableHeader() {
        return _driver.findElement(By.xpath(".//*/table[@class=\"recent-projects\"]/thead"));
    }

    // ������ ������� ��������
    public List<WebElement> projectTableDataStrings() {
        return _driver.findElements(By.xpath(".//*/div[@class=\"TMBodyMid\"]//table[@class=\"TMSection\"]/tbody/tr"));
    }

    // ���������� ����� �������
    public int getTableStringsCount() {
        List<WebElement> tab = projectTableDataStrings();
        return tab.size() - 1;
    }

    // �������� ������ ������� �� � ������ ������� � 1
    public WebElement getTableStringByNumber(int number) {
        return projectTableDataStrings().get(number);
    }

    // �������� ������ ������� �� ID �������
    public WebElement getTableStringByProjectID(String projectID) {
        List<WebElement> tableStrings = projectTableDataStrings();
        for (WebElement tabStr: tableStrings) {
            if (tabStr.findElement(By.xpath("td[4]")).getText().equals(projectID)) {
                return tabStr;
            }
        }
        return null;
    }

    // �������� ������ ������� �� ������������ �������
    public WebElement getTableStringByProjectName(String projectName) {
        List<WebElement> tableStrings = projectTableDataStrings();
        for (WebElement tabStr: tableStrings) {
            if (tabStr.findElement(By.xpath("td[5]")).getText().equals(projectName)) {
                return tabStr;
            }
        }
        return null;
    }

    // �������� ������� ������ ������� �� ������ �������
    public WebElement getTableElementFromString(WebElement tableString, int columnNumber) {
        return tableString.findElement(By.xpath("td[" + Integer.toString(columnNumber) +"]"));
    }

    // �������� ID ������� �� ������ �� � ������
    public String getProjectIdFromTableByStrNumber(int tabStrNumber) {
        return getTableStringByNumber(tabStrNumber).getAttribute("data-project_id").toString();
    }

    // �������� ID ������� �� ������ �� ������������ �������
    public String getProjectIDFromTableByProjectName(String projectName) {
        return getTableStringByProjectName(projectName).getAttribute("data-project_id").toString();
    }

    // ������ "����������� ������" � ������ �������
    public WebElement doubleProject(WebElement tableString) {
        return tableString.findElement(By.xpath("td[11]/[@onclick=\"$.ils.Log.welcome.copyProject(this)\"]"));
    }

    // ������ "������������� ������" � ������ �������
    public WebElement editProject(WebElement tableString) {
        return tableString.findElement(By.xpath("td[11]/[@onclick=\"$.ils.Log.welcome.projectDialog(this)\"]"));
    }

    // ������ "������� ������" � ������ �������
    public WebElement deleteProject(WebElement tableString) {
        return tableString.findElement(By.xpath("td[11]/[@onclick=\"$.ils.Log.welcome.deleteProject(this)\"]"));
    }

    // ���� �� ������ "����������� ������" � ������ �������
    public void doubleProjectClick(WebElement tableString) {
        doubleProject(tableString).click();
    }

    // ���� �� ������ "������������� ������" � ������ �������
    public void editProjectClick(WebElement tableString) {
        editProject(tableString).click();
    }

    // ���� �� ������ "������� ������" � ������ �������
    public void deleteProjectClick(WebElement tableString) {
        deleteProject(tableString).click();
        if (isAlertPresent()) {
            acceptAlert();
        }
    }

}
