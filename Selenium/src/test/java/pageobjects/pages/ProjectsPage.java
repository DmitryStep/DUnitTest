package pageobjects.pages;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.WebDriverWait;
import utils.CalendarManager;

import java.util.List;

public class ProjectsPage extends BasePage {

    public ProjectsPage(WebDriver driver, WebDriverWait waiter){
        super(driver, waiter);
    }

    // ---------------------------------- ProjectsPage WebElements ---------------------------------------------

    // ������ ����������� ��������
    public WebElement projectsPeriod() {
        return _driver.findElement(By.xpath(".//*[@id=\"propPeriod\"]"));
    }

    // ���������
    public WebElement Calendar() {
        return _driver.findElement(By.xpath(".//*[@class=\"month-wrapper\"]"));
    }

    // �������
    public WebElement projectTable() {
        return _driver.findElement(By.xpath(".//*/div[@class=\"TMBodyMid\"]"));
    }

    // ������ ������� ��������
    public List<WebElement> projectTableDataStrings() {
        return _driver.findElements(By.xpath(".//*/div[@class=\"TMBodyMid\"]//table[@class=\"TMSection\"]/tbody/tr"));
    }

    // ������ �����������
    public WebElement GroupList() {
        return _driver.findElement(By.xpath(".//*/div[contains(@class, \"TMSelect\")]"));
    }

    // ���� �����������
    public WebElement ResetGroupingMenu() {
        return _driver.findElement(By.xpath(".//*/div[contains(@class, \"TMMenuItemText\") and (text()=\"���\")]"));
    }
    // ---------------------------------- ProjectsPage Events --------------------------------------------------

    // ����� �����������
    public void ResetGrouping() {
        _waiter.until(ExpectedConditions.elementToBeClickable(GroupList()));
        GroupList().click();
         _waiter.until(ExpectedConditions.elementToBeClickable(ResetGroupingMenu()));
        ResetGroupingMenu().click();
    }

    // ���� �������
    public void setProjectsPeriod(String DateBegin, String DateEnd) {
        _waiter.until(ExpectedConditions.elementToBeClickable(projectsPeriod()));
        projectsPeriod().click();
        _waiter.until(ExpectedConditions.elementToBeClickable(Calendar()));
        CalendarManager cal = new CalendarManager(Calendar());
        cal.SetPeriod(DateBegin, DateEnd);
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
