/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
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

import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import javax.sql.DataSource;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.data.gemfire.LazyWiringDeclarableSupport;
import org.springframework.data.gemfire.config.GemfireConstants;
import org.springframework.data.gemfire.repository.sample.User;
import org.springframework.data.gemfire.test.support.DataSourceAdapter;
import org.springframework.stereotype.Repository;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheLoaderException;
import com.gemstone.gemfire.cache.LoaderHelper;
import com.gemstone.gemfire.cache.Region;

/**
 * The SpringContextBootstrappingInitializerTest class is a test suite of test cases testing the integrated
 * functionality of the SpringContextBootstrappingInitializer class.
 * <p/>
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
 * @since 1.3.4 (Spring Data GemFire)
 * @since 7.0.1 (GemFire)
 */
@SuppressWarnings("unused")
public class SpringContextBootstrappingInitializerIntegrationTest {

	protected static final String GEMFIRE_LOCATORS = "localhost[11235]";
	protected static final String GEMFIRE_LOG_LEVEL = "config";
	protected static final String GEMFIRE_MCAST_PORT = "0";
	protected static final String GEMFIRE_NAME = "SpringContextBootstrappingInitializationTest";
	protected static final String GEMFIRE_START_LOCATORS = "localhost[11235]";

	@Test
	public void testSpringContextBootstrappingInitialization() {
		Cache gemfireCache = new CacheFactory()
			.set("cache-xml-file", "cache-with-spring-context-bootstrap-initializer.xml")
			.set("locators", GEMFIRE_LOCATORS)
			.set("log-level", GEMFIRE_LOG_LEVEL)
			.set("mcast-port", GEMFIRE_MCAST_PORT)
			.set("name", GEMFIRE_NAME)
			.set("start-locator", GEMFIRE_LOCATORS)
			.create();

		assertNotNull("The GemFire Cache was not properly created or initialized!", gemfireCache);
		assertFalse("The GemFire Cache is close!", gemfireCache.isClosed());

		Set<Region<?, ?>> rootRegions = gemfireCache.rootRegions();

		assertNotNull(rootRegions);
		assertFalse(rootRegions.isEmpty());
		assertEquals(2, rootRegions.size());
		assertNotNull(gemfireCache.getRegion("/TestRegion"));
		assertNotNull(gemfireCache.getRegion("/Users"));

		ConfigurableApplicationContext applicationContext = SpringContextBootstrappingInitializer.getApplicationContext();

		assertNotNull(applicationContext);
		assertTrue(applicationContext.containsBean(GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME));
		assertTrue(applicationContext.containsBean("TestRegion"));
		assertFalse(applicationContext.containsBean("Users")); // Region 'Users' is defined in GemFire cache.xml
		assertTrue(applicationContext.containsBean("userDataSource"));
		assertTrue(applicationContext.containsBean("userDao"));
		assertTrue(applicationContext.containsBean("userService"));

		DataSource userDataSource = applicationContext.getBean("userDataSource", DataSource.class);
		TestUserDao userDao = applicationContext.getBean("userDao", TestUserDao.class);
		TestUserService userService = applicationContext.getBean("userService", TestUserService.class);

		assertSame(userDataSource, userDao.getDataSource());
		assertSame(userDao, userService.getUserDao());

		// NOTE a GemFire declared component initialized by Spring!
		UserDataStoreCacheLoader usersCacheLoader = UserDataStoreCacheLoader.getInstance();

		assertNotNull(usersCacheLoader);
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

	public static final class TestDataSource extends DataSourceAdapter {
	}

	@Repository
	public static interface UserDao {
	}

	public static final class TestUserDao implements UserDao {

		@Autowired
		private DataSource userDataSource;

		protected DataSource getDataSource() {
			Assert.state(userDataSource != null, "A reference to the Users DataSource as not properly configured!");
			return userDataSource;
		}
	}

	@Service
	public static interface UserService {
	}

	public static final class TestUserService implements UserService {

		@Autowired
		private UserDao userDao;

		protected UserDao getUserDao() {
			Assert.state(userDao != null, "A reference to the UserDao was not properly configured!");
			return userDao;
		}
	}

	public static final class UserDataStoreCacheLoader extends LazyWiringDeclarableSupport implements CacheLoader<String, User> {

		private static final AtomicReference<UserDataStoreCacheLoader> INSTANCE = new AtomicReference<UserDataStoreCacheLoader>();

		private static final Map<String, User> USER_DATA = new HashMap<String, User>(3);

		static {
			USER_DATA.put("jblum", new User("jblum"));
			USER_DATA.put("jdoe", new User("jdoe"));
			USER_DATA.put("jhandy", new User("jhandy"));
		}

		@Autowired
		private DataSource userDataSource;

		public static UserDataStoreCacheLoader getInstance() {
			return INSTANCE.get();
		}

		protected static User createUser(final String username) {
			return createUser(username, String.format("%1$s@xcompay.com", username), true, Calendar.getInstance());
		}

		protected static User createUser(final String username, final Boolean active) {
			return createUser(username, String.format("%1$s@xcompay.com", username), active, Calendar.getInstance());
		}

		protected static User createUser(final String username, final Boolean active, final Calendar since) {
			return createUser(username, String.format("%1$s@xcompay.com", username), active, since);
		}

		protected static User createUser(final String username, final String email, final Boolean active, final Calendar since) {
			User user = new User(username);
			user.setActive(active);
			user.setEmail(email);
			user.setSince(since);
			return user;
		}

		public UserDataStoreCacheLoader() {
			Assert.state(INSTANCE.compareAndSet(null, this), String.format("An instance of %1$s was already created!",
				getClass().getName()));
		}

		protected DataSource getDataSource() {
			return userDataSource;
		}

		@Override
		public void close() {
			USER_DATA.clear();
			userDataSource = null;
		}

		@Override
		public User load(final LoaderHelper<String, User> helper) throws CacheLoaderException {
			assertInitialized();
			return USER_DATA.get(helper.getKey());
		}
	}

}
