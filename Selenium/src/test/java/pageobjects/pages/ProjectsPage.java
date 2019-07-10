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

    // Кнопка "Создать новый проект"
    public WebElement newProjectButton(){
        return _driver.findElement(By.xpath(".//*/div[@class=\"ils-log-welcome\"]/*/button[@class=\"ils-button\"]"));
    }

    // Нажать кнопку "Создать новый проект"
    public void newProjectButtonClick() {
        newProjectButton().click();
    }

    // Дата начала периода фильтра
    public WebElement beginDate() {
        return _driver.findElement(By.xpath(".//*[@id=\"project_date1\"]"));
    }

    // Ввести дату начала периода
    public void typeBeginDate(String BeginDate) {
        beginDate().clear();
        beginDate().sendKeys(BeginDate);
    }

    // Дата окончания периода фильтра
    public WebElement endDate() {
        return _driver.findElement(By.xpath(".//*[@id=\"project_date2\"]"));
    }

    // Ввести дату окончания периода
    public void typeEndDate(String EndDate) {
        endDate().clear();
        endDate().sendKeys(EndDate);
    }

    // Кнопка "Показать"
    public WebElement showFilterResults() {
        return _driver.findElement(By.xpath(".//*/div[@class=\"ils-log-welcome-form\"]/*/button[@class=\"ils-button\"]"));
    }

    // Клик по кнопке "Показать"
    public void showFilterResultsClick() {
        showFilterResults().click();
    }

    // Заголовок таблицы проектов
    public WebElement projectsTableHeader() {
        return _driver.findElement(By.xpath(".//*/table[@class=\"recent-projects\"]/thead"));
    }

    // Строки таблицы проектов
    public List<WebElement> projectTableDataStrings() {
        return _driver.findElements(By.xpath(".//*/div[@class=\"TMBodyMid\"]//table[@class=\"TMSection\"]/tbody/tr"));
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
