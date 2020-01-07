/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Calendar;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import javax.sql.DataSource;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.LoaderHelper;
import org.apache.geode.cache.Region;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.repository.sample.User;
import org.springframework.data.gemfire.support.sample.TestUserDao;
import org.springframework.data.gemfire.support.sample.TestUserService;
import org.springframework.data.gemfire.test.support.DataSourceAdapter;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.Assert;

/**
 * The SpringContextBootstrappingInitializerTest class is a test suite of test cases testing the integrated
 * functionality of the SpringContextBootstrappingInitializer class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see LazyWiringDeclarableSupport
 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.CacheFactory
 * @see org.apache.geode.cache.CacheLoader
 * @see org.apache.geode.cache.Region
 * @since 1.4.0
 */
@SuppressWarnings("unused")
public class SpringContextBootstrappingInitializerIntegrationTest {

	private static final long CACHE_CLOSE_TIMEOUT = TimeUnit.SECONDS.toMillis(15);

	private static final Object MUTEX_LOCK = new Object();

	protected static final String GEMFIRE_LOCATORS = "localhost[11235]";
	protected static final String GEMFIRE_LOG_LEVEL = "error";
	protected static final String GEMFIRE_JMX_MANAGER = "true";
	protected static final String GEMFIRE_JMX_MANAGER_PORT = "1199";
	protected static final String GEMFIRE_JMX_MANAGER_START = "true";
	protected static final String GEMFIRE_MCAST_PORT = "0";
	protected static final String GEMFIRE_NAME = SpringContextBootstrappingInitializerIntegrationTest.class.getSimpleName();
	protected static final String GEMFIRE_START_LOCATORS = "localhost[11235]";

	@Before
	public void setup() {

		try {

			long timeout = System.currentTimeMillis() + CACHE_CLOSE_TIMEOUT;

			while (CacheFactory.getAnyInstance() != null && System.currentTimeMillis() < timeout) {
				synchronized (MUTEX_LOCK) {
					try {
						MUTEX_LOCK.wait(500L);
					}
					catch (InterruptedException ignore) { }
				}
			}

			fail(String.format("The Cache instance was not properly closed in the allotted timeout of %d seconds%n",
				TimeUnit.MILLISECONDS.toSeconds(CACHE_CLOSE_TIMEOUT)));
		}
		catch (CacheClosedException ignore) { }
	}

	@After
	public void tearDown() {

		SpringContextBootstrappingInitializer.getApplicationContext().close();
		UserDataStoreCacheLoader.INSTANCE.set(null);
		tearDownCache();
	}

	private void tearDownCache() {

		try {

			Cache cache = CacheFactory.getAnyInstance();

			if (cache != null) {

				cache.close();

				// Now, wait for the Apache Geode or Pivotal GemFire Hog to shutdown, OIY!
				synchronized (MUTEX_LOCK) {

					SpringUtils.safeRunOperation(() -> {
						while (!cache.isClosed()) {
							MUTEX_LOCK.wait(500L);
						}
					});

					MUTEX_LOCK.notifyAll();
				}
			}
		}
		catch (CacheClosedException ignore) {
			// CacheClosedExceptions happen when the Cache reference returned by GemFireCacheImpl.getInstance()
			// inside the CacheFactory.getAnyInstance() is null, or the Cache is already closed with calling
			// Cache.close();
		}
	}

	protected void doSpringContextBootstrappingInitializationTest(String cacheXmlFile) {

		Cache gemfireCache = new CacheFactory()
			.set("name", GEMFIRE_NAME)
			.set("log-level", GEMFIRE_LOG_LEVEL)
			.set("cache-xml-file", cacheXmlFile)
			//.set("start-locator", GEMFIRE_LOCATORS)
			//.set("jmx-manager", GEMFIRE_JMX_MANAGER)
			//.set("jmx-manager-port", GEMFIRE_JMX_MANAGER_PORT)
			//.set("jmx-manager-start", GEMFIRE_JMX_MANAGER_START)
			.create();

		assertNotNull("The cache was not properly created and initialized", gemfireCache);
		assertFalse("The cache is closed", gemfireCache.isClosed());

		Set<Region<?, ?>> rootRegions = gemfireCache.rootRegions();

		assertNotNull(rootRegions);
		assertFalse(rootRegions.isEmpty());
		assertEquals(2, rootRegions.size());
		assertNotNull(gemfireCache.getRegion("/TestRegion"));
		assertNotNull(gemfireCache.getRegion("/Users"));

		ConfigurableApplicationContext applicationContext =
			SpringContextBootstrappingInitializer.getApplicationContext();

		assertNotNull(applicationContext);
		assertTrue(applicationContext.containsBean(GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME));
		assertTrue(applicationContext.containsBean("TestRegion"));
		assertFalse(applicationContext.containsBean("Users")); // Region 'Users' is defined in Pivotal GemFire cache.xml
		assertTrue(applicationContext.containsBean("userDataSource"));
		assertTrue(applicationContext.containsBean("userDao"));
		assertTrue(applicationContext.containsBean("userService"));

		DataSource userDataSource = applicationContext.getBean("userDataSource", DataSource.class);
		TestUserDao userDao = applicationContext.getBean("userDao", TestUserDao.class);
		TestUserService userService = applicationContext.getBean("userService", TestUserService.class);

		assertSame(userDataSource, userDao.getDataSource());
		assertSame(userDao, userService.getUserDao());

		// NOTE Pivotal GemFire declared component initialized by Spring!
		UserDataStoreCacheLoader usersCacheLoader = UserDataStoreCacheLoader.getInstance();

		assertSame(userDataSource, usersCacheLoader.getDataSource());

		Region<String, User> users = gemfireCache.getRegion("/Users");

		assertNotNull(users);
		assertEquals("Users", users.getName());
		assertEquals("/Users", users.getFullPath());
		assertTrue(users.isEmpty());
		assertEquals(UserDataStoreCacheLoader.USER_DATA.get("jblum"), users.get("jblum"));
		assertEquals(UserDataStoreCacheLoader.USER_DATA.get("jdoe"), users.get("jdoe"));
		assertEquals(UserDataStoreCacheLoader.USER_DATA.get("jhandy"), users.get("jhandy"));
		assertFalse(users.isEmpty());
		assertEquals(3, users.size());
	}

	@Test
	public void testSpringContextBootstrappingInitializerUsingAnnotatedClasses() {

		SpringContextBootstrappingInitializer.register(TestAppConfig.class);

		new SpringContextBootstrappingInitializer().init(null, new Properties());

		ConfigurableApplicationContext applicationContext = SpringContextBootstrappingInitializer.getApplicationContext();

		UserDataStoreCacheLoader userDataStoreCacheLoader = applicationContext.getBean(UserDataStoreCacheLoader.class);
		DataSource userDataSource = applicationContext.getBean(DataSource.class);

		assertSame(UserDataStoreCacheLoader.getInstance(), userDataStoreCacheLoader);
		assertSame(userDataStoreCacheLoader.getDataSource(), userDataSource);
	}

	@Test
	public void testSpringContextBootstrappingInitializerUsingBasePackages() {
		doSpringContextBootstrappingInitializationTest(
			"cache-with-spring-context-bootstrap-initializer-using-base-packages.xml");
	}

	@Test
	public void testSpringContextBootstrappingInitializerUsingContextConfigLocations() {
		doSpringContextBootstrappingInitializationTest(
			"cache-with-spring-context-bootstrap-initializer.xml");
	}

	@Configuration
	public static class TestAppConfig {

		@Bean
		public DataSource userDataSource() {
			return new TestDataSource();
		}

		@Bean
		public UserDataStoreCacheLoader userDataStoreCacheLoader() {
			return new UserDataStoreCacheLoader();
		}
	}

	public static final class TestDataSource extends DataSourceAdapter { }

	public static final class UserDataStoreCacheLoader extends LazyWiringDeclarableSupport
			implements CacheLoader<String, User> {

		private static final AtomicReference<UserDataStoreCacheLoader> INSTANCE = new AtomicReference<>();

		private static final Map<String, User> USER_DATA = new ConcurrentHashMap<>(3);

		static {
			USER_DATA.put("jblum", new User("jblum"));
			USER_DATA.put("jdoe", new User("jdoe"));
			USER_DATA.put("jhandy", new User("jhandy"));
		}

		@Autowired
		private DataSource userDataSource;

		protected static User createUser(String username) {
			return createUser(username, true, Calendar.getInstance(), String.format("%1$s@xcompay.com", username));
		}

		protected static User createUser(String username, Boolean active) {
			return createUser(username, active, Calendar.getInstance(), String.format("%1$s@xcompay.com", username));
		}

		protected static User createUser(String username, Boolean active, Calendar since) {
			return createUser(username, active, since, String.format("%1$s@xcompay.com", username));
		}

		protected static User createUser(String username, Boolean active, Calendar since, String email) {

			User user = new User(username);
			user.setActive(active);
			user.setEmail(email);
			user.setSince(since);

			return user;
		}

		public static UserDataStoreCacheLoader getInstance() {
			return INSTANCE.get();
		}

		public UserDataStoreCacheLoader() {
			Assert.state(INSTANCE.compareAndSet(null, this),
				String.format("An instance of %1$s was already created!", getClass().getName()));
		}

		@Override
		protected void assertInitialized() {

			super.assertInitialized();

			Assert.state(this.userDataSource != null,
				String.format("The 'User' Data Source was not properly configured and initialized for use in (%s)",
					getClass().getName()));
		}

		protected DataSource getDataSource() {
			return this.userDataSource;
		}

		@Override
		public void close() {
			this.userDataSource = null;
		}

		@Override
		public User load(LoaderHelper<String, User> helper) throws CacheLoaderException {

			assertInitialized();

			return USER_DATA.get(helper.getKey());
		}
	}
}
