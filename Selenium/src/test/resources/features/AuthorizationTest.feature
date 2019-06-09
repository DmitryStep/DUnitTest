#language: ru

Функция: Авторизация

  @localize
  Сценарий: Переключение на английский язык на странице авторизации с попыткой неуспешной авторизации
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Выбрать язык English
        И Авторизоваться с логином 1 и паролем 1
    Тогда Текущая URL = http://saas.intelogis.ru/?lang=english
        И Активный язык English
        И Заголовок страницы = Login to the system
        И Подпись для поля Логин login
        И Подпись для поля Пароль password
        И Выведено сообщение Wrong login or password
        И Подпись на кнопке Авторизации Login

  @authorization
  Структура сценария: Неуспешная авторизация в системе с данными "<логин>" и "<пароль>", заголовок страницы "<заголовок>" и выведено сообщение о неверном логине или пароле
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Авторизоваться с логином <логин> и паролем <пароль>
    Тогда Заголовок страницы = <заголовок>
        И Выведено сообщение Неправильный логин или пароль
        И Текущая URL = http://saas.intelogis.ru/
        И Логин пустой
        И Пароль пустой
        И Заголовок страницы = Вход в систему

    Примеры:
      | логин           | пароль     | заголовок                                |
      | test@           | 4321       | Вход в систему                           |
      | test@katalon.ru | 54321      | Вход в систему                           |
      | test@katalon.ru |            | Вход в систему                           |
      |                 | 4321       | Вход в систему                           |
      |                 |            | Вход в систему                           |


  @authorization
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
    Тогда Текущая URL = http://saas.intelogis.ru/
        И Имя пользователя = test katalon
        И Заголовок страницы = Система мониторинга и планирования логистики
        И Подпись для ILS Plan = ПЛАНИРОВАНИЕ МАРШРУТОВ
        И Подпись для ILS Fact = МОНИТОРИНГ И ПЛАН-ФАКТНЫЙ АНАЛИЗ
        И Подпись для ILS Admin = ПАНЕЛЬ АДМИНИСТРАТОРА
        И Подпись для ILS Oper = ОПЕРАТИВНОЕ УПРАВЛЕНИЕ РЕСУРСАМИ
        И Активный язык Русский


  @localize
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 и последующим переключением на английский язык
    Дано  Адрес = http://saas.intelogis.ru/
    Если Авторизоваться с логином test@katalon.ru и паролем 4321
       И Выбрать язык English
    Тогда Текущая URL = http://saas.intelogis.ru/?lang=english
        И Имя пользователя = test katalon
        И Заголовок страницы = Monitoring and route planning system
        И Подпись для ILS Plan = ROUTE PLANNING
        И Подпись для ILS Fact = MONITORING AND PLAN-FACT ANALYSIS
        И Подпись для ILS Admin = ADMIN PANEL
        И Подпись для ILS Oper = OPERATIONAL RESOURCE MANAGEMENT
        И Активный язык English


  @authorization
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим выходом из системы
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Нажать меню пользователя
       И  Нажать Выход
    Тогда Текущая URL = http://saas.intelogis.ru/#
        И Логин пустой
        И Пароль пустой
        И Заголовок страницы = Вход в систему


  @authorization
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим выбором ILS Plan
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Нажать ILS Plan
    Тогда Текущая URL = http://saas.intelogis.ru/Log.htm
        И Активный язык Русский


  @authorization
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим выбором ILS Fact
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Нажать ILS Fact
    Тогда Текущая URL = http://saas.intelogis.ru/Mon.htm
        И Активный язык Русский


  @authorization
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим выбором ILS Admin
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Нажать ILS Admin
    Тогда Текущая URL = http://saas.intelogis.ru/Adm.htm
        И Активный язык Русский


  @authorization
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим выбором ILS Oper
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Нажать ILS Oper
    Тогда Текущая URL = http://saas.intelogis.ru/Opr.htm
        И Активный язык Русский


  @localize
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим переключением на Английский и выбором ILS Plan
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Выбрать язык English
       И  Нажать ILS Plan
    Тогда Текущая URL = http://saas.intelogis.ru/Log.htm?lang=english
        И Активный язык English


  @localize
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим переключением на Английский и выбором ILS Fact
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Выбрать язык English
       И  Нажать ILS Fact
    Тогда Текущая URL = http://saas.intelogis.ru/Mon.htm?lang=english
        И Активный язык English


  @localize
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим переключением на Английский и выбором ILS Admin
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Выбрать язык English
       И  Нажать ILS Admin
    Тогда Текущая URL = http://saas.intelogis.ru/Adm.htm?lang=english
        И Активный язык English


  @localize
  Сценарий: Успешная авторизация с логином test@katalon.ru и паролем 4321 с последующим переключением на Английский и выбором ILS Oper
    Дано  Адрес = http://saas.intelogis.ru/
    Если  Авторизоваться с логином test@katalon.ru и паролем 4321
       И  Выбрать язык English
       И  Нажать ILS Oper
    Тогда Текущая URL = http://saas.intelogis.ru/Opr.htm?lang=english
        И Активный язык English
