/*
 * Copyright 2016-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.io.Serializable;
import java.security.Principal;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;

import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.LoaderHelper;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.ServerOperationException;
import org.apache.geode.security.NotAuthorizedException;
import org.apache.geode.security.ResourcePermission;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Profile;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.AbstractAuthInitialize;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.data.gemfire.util.PropertiesBuilder;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Abstract base test class for implementing Apache Geode Integrated Security Integration Tests.
 *
 * @author John Blum
 * @see java.security.Principal
 * @see java.util.Properties
 * @see org.junit.FixMethodOrder
 * @see org.junit.Test
 * @see lombok
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAuthInitialize
 * @see org.springframework.data.gemfire.process.ProcessWrapper
 * @see org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport
 * @since 1.0.0
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
@SuppressWarnings("unused")
public abstract class AbstractGeodeSecurityIntegrationTests extends ClientServerIntegrationTestsSupport {

	protected static final int CACHE_SERVER_PORT = 13531;

	protected static final Logger LOGGER = LoggerFactory.getLogger(AbstractGeodeSecurityIntegrationTests.class);

	protected static final String CACHE_SERVER_HOST = "localhost";
	protected static final String GEODE_SECURITY_PROFILE_PROPERTY = "geode.security.profile";
	protected static final String SECURITY_PASSWORD_PROPERTY = "security-password";
	protected static final String SECURITY_USERNAME_PROPERTY = "security-username";

	private static final AtomicInteger RUN_COUNT = new AtomicInteger(0);

	private static ProcessWrapper geodeServerProcess;

	@BeforeClass
	public static void runGeodeServer() throws IOException {

		String geodeSecurityProfile = System.getProperty(GEODE_SECURITY_PROFILE_PROPERTY);

		if (StringUtils.hasText(geodeSecurityProfile)) {
			runGeodeServer(geodeSecurityProfile);
		}
	}

	protected static ProcessWrapper runGeodeServer(String geodeSecurityProfile) throws IOException {

		Assert.hasText(geodeSecurityProfile, String.format("[%s] System property is required",
			GEODE_SECURITY_PROFILE_PROPERTY));

		String debugEndpoint = Boolean.getBoolean(DEBUGGING_ENABLED_PROPERTY) ? DEBUG_ENDPOINT : null;

		geodeServerProcess = run(GeodeServerConfiguration.class,
			String.format("-Dgemfire.log-file=%s", logFile()),
			String.format("-Dgemfire.log-level=%s", logLevel(TEST_GEMFIRE_LOG_LEVEL)),
			String.format("-Dspring.profiles.active=apache-geode-server,%s", geodeSecurityProfile),
			debugEndpoint);

		waitForServerToStart(CACHE_SERVER_HOST, CACHE_SERVER_PORT);

		return geodeServerProcess;
	}

	@AfterClass
	public static void stopGeodeServer() {
		System.clearProperty(GEODE_SECURITY_PROFILE_PROPERTY);
		stop(geodeServerProcess);
	}

	@Resource(name = "Echo")
	private Region<String, String> echo;

	@Test
	@DirtiesContext
	public void authorizedUser() {

		assertThat(echo.get("one")).isEqualTo("one");
		assertThat(echo.put("two", "four")).isNull();
		assertThat(echo.get("two")).isEqualTo("four");
	}

	@Test(expected = NotAuthorizedException.class)
	public void unauthorizedUser() {

		try {
			assertThat(echo.get("one")).isEqualTo("one");
			echo.put("two", "four");
		}
		catch (ServerOperationException expected) {

			assertThat(expected).hasMessageContaining("analyst not authorized for DATA:WRITE:Echo:two");
			assertThat(expected).hasCauseInstanceOf(NotAuthorizedException.class);

			throw (NotAuthorizedException) expected.getCause();
		}
		finally {
			assertThat(echo).doesNotContainKey("two");
		}
	}

	public static class GeodeClientAuthInitialize extends AbstractAuthInitialize {

		protected static final User ANALYST = User.newUser("analyst").with("p@55w0rd");
		protected static final User SCIENTIST = User.newUser("scientist").with("w0rk!ng4u");

		private final User user;

		/* (non-Javadoc) */
		public static GeodeClientAuthInitialize create() {
			return new GeodeClientAuthInitialize(RUN_COUNT.incrementAndGet() < 2 ? SCIENTIST : ANALYST);
		}

		/* (non-Javadoc) */
		public GeodeClientAuthInitialize(User user) {
			Assert.notNull(user, "User cannot be null");
			this.user = user;
		}

		/* (non-Javadoc) */
		@Override
		protected Properties doGetCredentials(Properties securityProperties) {

			User user = getUser();

			return new PropertiesBuilder()
				.setProperty(SECURITY_USERNAME_PROPERTY, user.getName())
				.setProperty(SECURITY_PASSWORD_PROPERTY, user.getCredentials())
				.build();
		}

		/* (non-Javadoc) */
		protected User getUser() {
			return this.user;
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public String toString() {

			User user = getUser();

			return String.format("%1$s:%2$s", user.getName(), user.getCredentials());
		}
	}

	@ClientCacheApplication(name = "GeodeSecurityIntegrationTestsClient", logLevel = "error",
		servers = { @ClientCacheApplication.Server(port = CACHE_SERVER_PORT) })
	@EnableAuth(clientAuthenticationInitializer =
		"org.springframework.data.gemfire.config.annotation.AbstractGeodeSecurityIntegrationTests$GeodeClientAuthInitialize.create")
	@Profile("apache-geode-client")
	static class GeodeClientConfiguration {

		@Bean("Echo")
		ClientRegionFactoryBean<String, String> echoRegion(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<String, String> echoRegion = new ClientRegionFactoryBean<>();

			echoRegion.setCache(gemfireCache);
			echoRegion.setClose(false);
			echoRegion.setShortcut(ClientRegionShortcut.PROXY);

			return echoRegion;
		}
	}

	@CacheServerApplication(name = "GeodeSecurityIntegrationTestsServer", logLevel = "error",
		port = CACHE_SERVER_PORT)
	@Import({
		ApacheShiroIniSecurityIntegrationTests.ApacheShiroIniConfiguration.class,
		ApacheShiroRealmSecurityIntegrationTests.ApacheShiroRealmConfiguration.class,
		ApacheGeodeSecurityManagerSecurityIntegrationTests.ApacheGeodeSecurityManagerConfiguration.class
	})
	@Profile("apache-geode-server")
	public static class GeodeServerConfiguration {

		public static void main(String[] args) {
			runSpringApplication(GeodeServerConfiguration.class, args);
		}

		@Autowired
		private GemFireCache gemfireCache;

		@Bean("Echo")
		LocalRegionFactoryBean<String, String> echoRegion(GemFireCache gemfireCache) {

			LocalRegionFactoryBean<String, String> echoRegion = new LocalRegionFactoryBean<>();

			echoRegion.setCache(gemfireCache);
			echoRegion.setCacheLoader(echoCacheLoader());
			echoRegion.setClose(false);
			echoRegion.setPersistent(false);

			return echoRegion;
		}

		CacheLoader<String, String> echoCacheLoader() {

			return new CacheLoader<String, String>() {

				@Override
				public String load(LoaderHelper<String, String> helper) throws CacheLoaderException {
					return helper.getKey();
				}

				@Override
				public void close() {
				}
			};
		}

		@PostConstruct
		public void postProcess() {
			if (LOGGER.isInfoEnabled()) {
				LOGGER.info("Geode Distributed System Properties [{}]",
					CollectionUtils.toString(gemfireCache.getDistributedSystem().getProperties()));
			}
		}
	}

	@Getter
	@EqualsAndHashCode(of = { "name", "credentials" })
	@RequiredArgsConstructor(staticName = "newUser")
	@SuppressWarnings("all")
	public static class User implements Iterable<Role>, Principal, Serializable {

		private Set<Role> roles = new HashSet<>();

		@NonNull
		private String name;
		private String credentials;

		/* (non-Javadoc) */
		public boolean hasPermission(ResourcePermission permission) {

			for (Role role : this) {
				if (role.hasPermission(permission)) {
					return true;
				}
			}

			return false;
		}

		/* (non-Javadoc) */
		public boolean hasRole(Role role) {
			return this.roles.contains(role);
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public Iterator<Role> iterator() {
			return Collections.unmodifiableSet(this.roles).iterator();
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public String toString() {
			return getName();
		}

		/* (non-Javadoc) */
		public User with(String credentials) {

			this.credentials = credentials;

			return this;
		}

		/* (non-Javadoc) */
		public User with(Role... roles) {

			Collections.addAll(this.roles, roles);

			return this;
		}
	}

	@Getter
	@EqualsAndHashCode(of = "name")
	@RequiredArgsConstructor(staticName = "newRole")
	@SuppressWarnings("unsed")
	public static class Role implements Iterable<ResourcePermission>, Serializable {

		@NonNull
		private String name;

		private Set<ResourcePermission> permissions = new HashSet<>();

		/* (non-Javadoc) */
		public boolean hasPermission(ResourcePermission permission) {

			for (ResourcePermission thisPermission : this) {
				if (thisPermission.implies(permission)) {
					return true;
				}
			}

			return false;
		}

		/**
		 * @inheritDoc
		 */
		@Override
		@SuppressWarnings("all")
		public Iterator<ResourcePermission> iterator() {
			return Collections.unmodifiableSet(this.permissions).iterator();
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public String toString() {
			return getName();
		}

		/* (non-Javadoc) */
		public Role with(ResourcePermission... permissions) {

			Collections.addAll(this.permissions, permissions);

			return this;
		}
	}
}
