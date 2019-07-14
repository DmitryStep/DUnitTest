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

    // Период отображения проектов
    public WebElement projectsPeriod() {
        return _driver.findElement(By.xpath(".//*[@id=\"propPeriod\"]"));
    }

    // Календарь
    public WebElement Calendar() {
        return _driver.findElement(By.xpath(".//*[@class=\"month-wrapper\"]"));
    }

    // Таблица
    public WebElement projectTable() {
        return _driver.findElement(By.xpath(".//*/div[@class=\"TMBodyMid\"]"));
    }

    // Строки таблицы проектов
    public List<WebElement> projectTableDataStrings() {
        return _driver.findElements(By.xpath(".//*/div[@class=\"TMBodyMid\"]//table[@class=\"TMSection\"]/tbody/tr"));
    }

    // Список группировки
    public WebElement GroupList() {
        return _driver.findElement(By.xpath(".//*/div[contains(@class, \"TMSelect\")]"));
    }

    // Меню группировки
    public WebElement ResetGroupingMenu() {
        return _driver.findElement(By.xpath(".//*/div[contains(@class, \"TMMenuItemText\") and (text()=\"нет\")]"));
    }
    // ---------------------------------- ProjectsPage Events --------------------------------------------------

    // Сброс группировки
    public void ResetGrouping() {
        _waiter.until(ExpectedConditions.elementToBeClickable(GroupList()));
        GroupList().click();
         _waiter.until(ExpectedConditions.elementToBeClickable(ResetGroupingMenu()));
        ResetGroupingMenu().click();
    }

    // Ввод периода
    public void setProjectsPeriod(String DateBegin, String DateEnd) {
        _waiter.until(ExpectedConditions.elementToBeClickable(projectsPeriod()));
        projectsPeriod().click();
        _waiter.until(ExpectedConditions.elementToBeClickable(Calendar()));
        CalendarManager cal = new CalendarManager(Calendar());
        cal.SetPeriod(DateBegin, DateEnd);
    }

    // Количество строк таблицы
    public int getTableStringsCount() {
        List<WebElement> tab = projectTableDataStrings();
        return tab.size() - 1;
    }

    // Получить строку таблицы по её номеру начиная с 1
    public WebElement getTableStringByNumber(int number) {

        return projectTableDataStrings().get(number);
    }

    // Получить строку таблицы по ID проекта
    public WebElement getTableStringByProjectID(String projectID) {
        List<WebElement> tableStrings = projectTableDataStrings();
        for (WebElement tabStr: tableStrings) {
            if (tabStr.findElement(By.xpath("td[4]")).getText().equals(projectID)) {
                return tabStr;
            }
        }
        return null;
    }

    // Получить строку таблицы по Наименованию проекта
    public WebElement getTableStringByProjectName(String projectName) {
        List<WebElement> tableStrings = projectTableDataStrings();
        for (WebElement tabStr: tableStrings) {
            if (tabStr.findElement(By.xpath("td[5]")).getText().equals(projectName)) {
                return tabStr;
            }
        }
        return null;
    }

    // Получить элемент строки таблицы по номеру столбца
    public WebElement getTableElementFromString(WebElement tableString, int columnNumber) {
        return tableString.findElement(By.xpath("td[" + Integer.toString(columnNumber) +"]"));
    }

    // Получить ID проекта из строки по её номеру
    public String getProjectIdFromTableByStrNumber(int tabStrNumber) {
        return getTableStringByNumber(tabStrNumber).getAttribute("data-project_id").toString();
    }

    // Получить ID проекта из строки по наименованию проекта
    public String getProjectIDFromTableByProjectName(String projectName) {
        return getTableStringByProjectName(projectName).getAttribute("data-project_id").toString();
    }

    // Кнопка "Дублировать проект" в строке таблицы
    public WebElement doubleProject(WebElement tableString) {
        return tableString.findElement(By.xpath("td[11]/[@onclick=\"$.ils.Log.welcome.copyProject(this)\"]"));
    }

    // Кнопка "Редактировать проект" в строке таблицы
    public WebElement editProject(WebElement tableString) {
        return tableString.findElement(By.xpath("td[11]/[@onclick=\"$.ils.Log.welcome.projectDialog(this)\"]"));
    }

    // Кнопка "Удалить проект" в строке таблицы
    public WebElement deleteProject(WebElement tableString) {
        return tableString.findElement(By.xpath("td[11]/[@onclick=\"$.ils.Log.welcome.deleteProject(this)\"]"));
    }

    // Клик по кнопке "Дублировать проект" в строке таблицы
    public void doubleProjectClick(WebElement tableString) {
        doubleProject(tableString).click();
    }

    // Клик по кнопке "Редактировать проект" в строке таблицы
    public void editProjectClick(WebElement tableString) {
        editProject(tableString).click();
    }

    // Клик по кнопке "Удалить проект" в строке таблицы
    public void deleteProjectClick(WebElement tableString) {
        deleteProject(tableString).click();
        if (isAlertPresent()) {
            acceptAlert();
        }
    }

}
