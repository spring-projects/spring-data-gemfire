/*
 * Copyright 2017 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.annotation.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.resource.ResourceException;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.ra.GFConnection;
import org.apache.geode.ra.GFConnectionFactory;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.config.annotation.EnableGemFireAsLastResource;
import org.springframework.data.gemfire.config.annotation.GemFireAsLastResourceConfiguration;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.transaction.annotation.Transactional;

/**
 * Integration tests for {@link EnableGemFireAsLastResource}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.apache.geode.ra.GFConnection
 * @see org.apache.geode.ra.GFConnectionFactory
 * @see org.springframework.data.gemfire.config.annotation.EnableGemFireAsLastResource
 * @see org.springframework.data.gemfire.config.annotation.GemFireAsLastResourceConfiguration
 * @see org.springframework.transaction.PlatformTransactionManager
 * @see org.springframework.transaction.annotation.EnableTransactionManagement
 * @see org.springframework.transaction.annotation.Transactional
 * @since 2.0.0
 */
public class EnableGemFireAsLastResourceIntegrationTests {

	private static List<SpringGemFireTransactionEvents> transactionEvents = new ArrayList<>();

	@Before
	public void setup() {
		transactionEvents.clear();
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		ConfigurableApplicationContext applicationContext = new AnnotationConfigApplicationContext(annotatedClasses);
		applicationContext.registerShutdownHook();
		return applicationContext;
	}

	@Test
	public void configurationIsCorrect() {

		ConfigurableApplicationContext applicationContext =
			newApplicationContext(TestGemFireAsLastResourceConfiguration.class);

		assertThat(applicationContext).isNotNull();

		GemFireCache gemfireCache = applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getCopyOnRead()).isTrue();

		GemFireAsLastResourceConnectionAcquiringAspect connectionAcquiringAspect =
			applicationContext.getBean(GemFireAsLastResourceConnectionAcquiringAspect.class);

		assertThat(connectionAcquiringAspect).isNotNull();
		assertThat(connectionAcquiringAspect.getOrder()).isEqualTo(3);

		GemFireAsLastResourceConnectionClosingAspect connectionClosingAspect =
			applicationContext.getBean(GemFireAsLastResourceConnectionClosingAspect.class);

		assertThat(connectionClosingAspect).isNotNull();
		assertThat(connectionClosingAspect.getOrder()).isEqualTo(1);
	}

	private void transactionEventsForTransactionalServiceAreCorrect(
			Class<? extends TestTransactionalService> transactionalServiceType) {

		ConfigurableApplicationContext applicationContext =
			newApplicationContext(TestSpringApplicationConfiguration.class);

		assertThat(applicationContext).isNotNull();
		assertThat(applicationContext.containsBean(transactionalServiceType.getSimpleName())).isTrue();
		assertThat(transactionEvents).isEmpty();

		GFConnectionFactory gemfireConnectionFactory = applicationContext.getBean(GFConnectionFactory.class);

		assertThat(gemfireConnectionFactory).isNotNull();

		GemFireAsLastResourceConnectionAcquiringAspect connectionAcquiringAspect =
			applicationContext.getBean(GemFireAsLastResourceConnectionAcquiringAspect.class);

		assertThat(connectionAcquiringAspect).isNotNull();
		assertThat(connectionAcquiringAspect.getGemFireConnectionFactory()).isSameAs(gemfireConnectionFactory);

		TestTransactionalService service =
			applicationContext.getBean(transactionalServiceType.getSimpleName(), transactionalServiceType);

		assertThat(service).isNotNull();

		service.doInTransactionCommits();

		assertThat(transactionEvents).containsExactly(SpringGemFireTransactionEvents.BEGIN,
			SpringGemFireTransactionEvents.GET_CONNECTION,
			SpringGemFireTransactionEvents.TRANSACTION,
			SpringGemFireTransactionEvents.COMMIT,
			SpringGemFireTransactionEvents.CLOSE_CONNECTION);

		transactionEvents.clear();

		assertThat(transactionEvents).isEmpty();

		try {
			service.doInTransactionRollsback();
		}
		catch (RuntimeException expected) {
			assertThat(expected).hasMessage("TEST");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			assertThat(transactionEvents).containsExactly(SpringGemFireTransactionEvents.BEGIN,
				SpringGemFireTransactionEvents.GET_CONNECTION,
				SpringGemFireTransactionEvents.TRANSACTION,
				SpringGemFireTransactionEvents.ROLLBACK,
				SpringGemFireTransactionEvents.CLOSE_CONNECTION);
		}
	}

	@Test(expected = RuntimeException.class)
	public void transactionEventsForTransactionalServiceClassAreCorrect() {
		transactionEventsForTransactionalServiceAreCorrect(TestTransactionalServiceClass.class);
	}

	@Test(expected = RuntimeException.class)
	public void transactionEventsForTransactionalServiceMethodsAreCorrect() {
		transactionEventsForTransactionalServiceAreCorrect(TestTransactionalServiceMethods.class);
	}

	@Test(expected = IllegalStateException.class)
	public void missingEnableTransactionManagerAnnotationThrowsIllegalStateException() {
		try {
			newApplicationContext(TestMissingEnableTransactionManagementAnnotationConfiguration.class);
		}
		catch (BeanCreationException expected) {

			assertThat(expected).hasMessageStartingWith(String.format("Error creating bean with name '%s'",
				GemFireAsLastResourceConfiguration.class.getName()));

			assertThat(expected).hasCauseInstanceOf(IllegalStateException.class);

			assertThat(expected.getCause()).hasMessage("The @EnableGemFireAsLastResource annotation may only be used"
				+ " on a Spring application @Configuration class that is also annotated with"
				+ " @EnableTransactionManagement having an explicit [order] set");

			assertThat(expected.getCause()).hasNoCause();

			throw (IllegalStateException) expected.getCause();
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void missingEnableTransactionManagementOrderAttributeConfigurationThrowsIllegalArgumentException() {
		try {
			newApplicationContext(TestMissingEnableTransactionManagementOrderAttributeConfiguration.class);
		}
		catch (BeanCreationException expected) {

			assertThat(expected).hasMessageStartingWith(String.format("Error creating bean with name '%s'",
				GemFireAsLastResourceConfiguration.class.getName()));

			assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);

			assertThat(expected.getCause()).hasMessage("The @EnableTransactionManagement(order) attribute value"
				+ " [2147483647] must be explicitly set to a value other than Integer.MAX_VALUE or Integer.MIN_VALUE");

			assertThat(expected.getCause()).hasNoCause();

			throw (IllegalArgumentException) expected.getCause();
		}
	}

	@Configuration
	@EnableGemFireAsLastResource
	@EnableTransactionManagement(order = 2)
	static class TestGemFireAsLastResourceConfiguration {

		@Bean("gemfireCache")
		GemFireCache mockGemFireCache() {

			AtomicBoolean copyOnRead = new AtomicBoolean(false);

			GemFireCache mockGemFireCache = mock(GemFireCache.class);

			doAnswer(invocation -> {
				copyOnRead.set(invocation.getArgument(0));
				return null;
			}).when(mockGemFireCache).setCopyOnRead(anyBoolean());

			when(mockGemFireCache.getCopyOnRead()).thenAnswer(invocation -> copyOnRead.get());

			return mockGemFireCache;
		}

		@Bean
		GFConnectionFactory mockGemFireConnectionFactory() throws ResourceException {

			GFConnectionFactory mockGemFireConnectionFactory = mock(GFConnectionFactory.class);

			GFConnection mockGemFireConnection = mock(GFConnection.class);

			when(mockGemFireConnectionFactory.getConnection()).thenAnswer(invocation -> {
				transactionEvents.add(SpringGemFireTransactionEvents.GET_CONNECTION);
				return mockGemFireConnection;
			});

			doAnswer(invocation -> transactionEvents.add(SpringGemFireTransactionEvents.CLOSE_CONNECTION))
				.when(mockGemFireConnection).close();

			return mockGemFireConnectionFactory;
		}

		@Bean("transactionManager")
		PlatformTransactionManager mockTransactionManager() {

			PlatformTransactionManager mockTransactionManager = mock(PlatformTransactionManager.class);

			when(mockTransactionManager.getTransaction(any(TransactionDefinition.class))).thenAnswer(invocation -> {

				TransactionStatus mockTransactionStatus = mock(TransactionStatus.class);

				transactionEvents.add(SpringGemFireTransactionEvents.BEGIN);

				return mockTransactionStatus;
			});

			doAnswer(invocation -> transactionEvents.add(SpringGemFireTransactionEvents.COMMIT))
				.when(mockTransactionManager).commit(any(TransactionStatus.class));

			doAnswer(invocation -> transactionEvents.add(SpringGemFireTransactionEvents.ROLLBACK))
				.when(mockTransactionManager).rollback(any(TransactionStatus.class));

			return mockTransactionManager;
		}
	}

	@Configuration
	@EnableGemFireAsLastResource
	static class TestMissingEnableTransactionManagementAnnotationConfiguration {
	}

	@Configuration
	@EnableGemFireAsLastResource
	@EnableTransactionManagement
	static class TestMissingEnableTransactionManagementOrderAttributeConfiguration {

		@Bean("transactionManager")
		PlatformTransactionManager mockTransactionManager() {
			return mock(PlatformTransactionManager.class);
		}
	}

	@Configuration
	@Import(TestGemFireAsLastResourceConfiguration.class)
	static class TestSpringApplicationConfiguration {

		@Bean("TestTransactionalServiceClass")
		TestTransactionalServiceClass testTransactionalServiceClass() {
			return new TestTransactionalServiceClass();
		}

		@Bean("TestTransactionalServiceMethods")
		TestTransactionalServiceMethods testTransactionalServiceMethods() {
			return new TestTransactionalServiceMethods();
		}
	}

	enum SpringGemFireTransactionEvents {

		BEGIN,
		CLOSE_CONNECTION,
		COMMIT,
		GET_CONNECTION,
		ROLLBACK,
		TRANSACTION,

	}

	interface TestTransactionalService {

		void doInTransactionCommits();

		void doInTransactionRollsback();

	}

	@Service("TestTransactionalServiceClass")
	@Transactional
	static class TestTransactionalServiceClass implements TestTransactionalService {

		public void doInTransactionCommits() {
			transactionEvents.add(SpringGemFireTransactionEvents.TRANSACTION);
		}

		public void doInTransactionRollsback() {
			transactionEvents.add(SpringGemFireTransactionEvents.TRANSACTION);
			throw new RuntimeException("TEST");
		}
	}

	@Service("TestTransactionalServiceMethods")
	static class TestTransactionalServiceMethods implements TestTransactionalService {

		@Transactional
		public void doInTransactionCommits() {
			transactionEvents.add(SpringGemFireTransactionEvents.TRANSACTION);
		}

		@Transactional
		public void doInTransactionRollsback() {
			transactionEvents.add(SpringGemFireTransactionEvents.TRANSACTION);
			throw new RuntimeException("TEST");
		}
	}
}
