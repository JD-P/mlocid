#!/usr/bin/env python3
"""
Selenium tests for mlocid frontend
Requires: selenium, webdriver-manager
Install: pip install selenium webdriver-manager
"""

import time
import unittest
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from webdriver_manager.chrome import ChromeDriverManager


class MlocidFrontendTests(unittest.TestCase):
    base_url = "http://localhost:8080"
    
    @classmethod
    def setUpClass(cls):
        """Set up the browser once for all tests"""
        chrome_options = Options()
        chrome_options.add_argument("--headless")  # Run in headless mode
        chrome_options.add_argument("--no-sandbox")
        chrome_options.add_argument("--disable-dev-shm-usage")
        service = Service(ChromeDriverManager().install())
        cls.driver = webdriver.Chrome(service=service, options=chrome_options)
        cls.driver.implicitly_wait(10)
    
    @classmethod
    def tearDownClass(cls):
        """Close the browser after all tests"""
        cls.driver.quit()
    
    def setUp(self):
        """Navigate to base URL before each test"""
        self.driver.get(self.base_url)
    
    def test_homepage_loads(self):
        """Test that the homepage loads correctly"""
        self.assertIn("mlocid", self.driver.title)
        self.assertIn("Welcome to mlocid", self.driver.page_source)
    
    def test_navigation_to_login(self):
        """Test navigation to login page"""
        login_link = self.driver.find_element(By.LINK_TEXT, "Login")
        login_link.click()
        WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.ID, "loginForm"))
        )
        self.assertIn("Login", self.driver.title)
        self.assertIsNotNone(self.driver.find_element(By.ID, "username"))
        self.assertIsNotNone(self.driver.find_element(By.ID, "password"))
    
    def test_navigation_to_register(self):
        """Test navigation to register page"""
        register_link = self.driver.find_element(By.LINK_TEXT, "Register")
        register_link.click()
        WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.ID, "registerForm"))
        )
        self.assertIn("Register", self.driver.title)
        self.assertIsNotNone(self.driver.find_element(By.ID, "username"))
        self.assertIsNotNone(self.driver.find_element(By.ID, "password"))
    
    def test_register_new_user(self):
        """Test user registration"""
        register_link = self.driver.find_element(By.LINK_TEXT, "Register")
        register_link.click()
        WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.ID, "registerForm"))
        )
        
        username = f"testuser_{int(time.time())}"
        username_field = self.driver.find_element(By.ID, "username")
        password_field = self.driver.find_element(By.ID, "password")
        submit_button = self.driver.find_element(By.CSS_SELECTOR, "button[type='submit']")
        
        username_field.send_keys(username)
        password_field.send_keys("testpass123")
        submit_button.click()
        
        # Wait for redirect to cards page
        WebDriverWait(self.driver, 10).until(
            lambda d: "cards" in d.current_url or "error" in d.page_source.lower()
        )
        
        # Should redirect to cards page on success
        self.assertIn("cards", self.driver.current_url)
    
    def test_login_flow(self):
        """Test login functionality"""
        # First register a user
        register_link = self.driver.find_element(By.LINK_TEXT, "Register")
        register_link.click()
        WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.ID, "registerForm"))
        )
        
        username = f"testuser_{int(time.time())}"
        username_field = self.driver.find_element(By.ID, "username")
        password_field = self.driver.find_element(By.ID, "password")
        submit_button = self.driver.find_element(By.CSS_SELECTOR, "button[type='submit']")
        
        username_field.send_keys(username)
        password_field.send_keys("testpass123")
        submit_button.click()
        
        # Wait for redirect
        WebDriverWait(self.driver, 10).until(
            lambda d: "cards" in d.current_url
        )
        
        # Logout
        logout_link = self.driver.find_element(By.ID, "logout")
        logout_link.click()
        
        # Wait for redirect to login
        WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.ID, "loginForm"))
        )
        
        # Login again
        username_field = self.driver.find_element(By.ID, "username")
        password_field = self.driver.find_element(By.ID, "password")
        submit_button = self.driver.find_element(By.CSS_SELECTOR, "button[type='submit']")
        
        username_field.send_keys(username)
        password_field.send_keys("testpass123")
        submit_button.click()
        
        # Should redirect to cards page
        WebDriverWait(self.driver, 10).until(
            lambda d: "cards" in d.current_url
        )
        self.assertIn("cards", self.driver.current_url)
    
    def test_create_flashcard(self):
        """Test creating a new flashcard"""
        # Register and login first
        register_link = self.driver.find_element(By.LINK_TEXT, "Register")
        register_link.click()
        WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.ID, "registerForm"))
        )
        
        username = f"testuser_{int(time.time())}"
        username_field = self.driver.find_element(By.ID, "username")
        password_field = self.driver.find_element(By.ID, "password")
        submit_button = self.driver.find_element(By.CSS_SELECTOR, "button[type='submit']")
        
        username_field.send_keys(username)
        password_field.send_keys("testpass123")
        submit_button.click()
        
        # Wait for cards page
        WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.ID, "newCardBtn"))
        )
        
        # Click new card button
        new_card_btn = self.driver.find_element(By.ID, "newCardBtn")
        new_card_btn.click()
        
        # Wait for modal
        WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.ID, "cardModal"))
        )
        
        # Fill in form
        question_field = self.driver.find_element(By.ID, "question")
        answer_field = self.driver.find_element(By.ID, "answer")
        save_button = self.driver.find_element(By.ID, "saveCardBtn")
        
        question_field.send_keys("What is 2+2?")
        answer_field.send_keys("4")
        save_button.click()
        
        # Wait for modal to close and card to appear
        WebDriverWait(self.driver, 10).until(
            EC.invisibility_of_element_located((By.ID, "cardModal"))
        )
        
        # Verify card appears in list
        time.sleep(1)  # Give time for DOM update
        self.assertIn("What is 2+2?", self.driver.page_source)
        self.assertIn("4", self.driver.page_source)
    
    def test_study_page_navigation(self):
        """Test navigation to study page"""
        # Register and login first
        register_link = self.driver.find_element(By.LINK_TEXT, "Register")
        register_link.click()
        WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.ID, "registerForm"))
        )
        
        username = f"testuser_{int(time.time())}"
        username_field = self.driver.find_element(By.ID, "username")
        password_field = self.driver.find_element(By.ID, "password")
        submit_button = self.driver.find_element(By.CSS_SELECTOR, "button[type='submit']")
        
        username_field.send_keys(username)
        password_field.send_keys("testpass123")
        submit_button.click()
        
        # Wait for cards page
        WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.ID, "newCardBtn"))
        )
        
        # Navigate to study page
        study_link = self.driver.find_element(By.LINK_TEXT, "Study")
        study_link.click()
        
        # Should be on study page
        WebDriverWait(self.driver, 10).until(
            lambda d: "study" in d.current_url
        )
        self.assertIn("study", self.driver.current_url)


if __name__ == "__main__":
    unittest.main()
