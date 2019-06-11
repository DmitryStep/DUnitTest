#language: ru

Функция: Авторизация - локализация

@localization
  Сценарий: Переключение на английский язык на странице авторизации с попыткой неуспешной авторизации
    Дано  Адрес = http://saas.intelogis.ru
    Если  Выбрать язык English
       И  Авторизоваться с логином 1 и паролем 1
    Тогда Текущая URL = http://saas.intelogis.ru/?lang=english
        И Активный язык English
        И Заголовок страницы = Login to the system
        И Подпись для поля Логин login
        И Подпись для поля Пароль password
        И Выведено сообщение Wrong login or password
        И Подпись на кнопке Авторизации Login

  @localization
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим переключением на Английский и выбором ILS Plan
    Дано  Адрес = http://saas.intelogis.ru
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Выбрать язык English
       И  Нажать ILS Plan
    Тогда Текущая URL = http://saas.intelogis.ru/Log.htm?lang=english
        И Активный язык English


  @localization
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим переключением на Английский и выбором ILS Fact
    Дано  Адрес = http://saas.intelogis.ru
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Выбрать язык English
       И  Нажать ILS Fact
    Тогда Текущая URL = http://saas.intelogis.ru/Mon.htm?lang=english
        И Активный язык English


  @localization
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим переключением на Английский и выбором ILS Admin
    Дано  Адрес = http://saas.intelogis.ru
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Выбрать язык English
       И  Нажать ILS Admin
    Тогда Текущая URL = http://saas.intelogis.ru/Adm.htm?lang=english
        И Активный язык English


  @localization
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим переключением на Английский и выбором ILS Oper
    Дано  Адрес = http://saas.intelogis.ru
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Выбрать язык English
       И  Нажать ILS Oper
    Тогда Текущая URL = http://saas.intelogis.ru/Opr.htm?lang=english
        И Активный язык English


  @localization
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 и последующим переключением на английский язык
    Дано  Адрес = http://saas.intelogis.ru
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Выбрать язык English
    Тогда Текущая URL = http://saas.intelogis.ru/?lang=english
        И Имя пользователя = test katalon
        И Заголовок страницы = Monitoring and route planning system
        И Подпись для ILS Plan = ROUTE PLANNING
        И Подпись для ILS Fact = MONITORING AND PLAN-FACT ANALYSIS
        И Подпись для ILS Admin = ADMIN PANEL
        И Подпись для ILS Oper = OPERATIONAL RESOURCE MANAGEMENT
        И Активный язык English