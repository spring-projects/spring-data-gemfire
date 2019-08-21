/*
 * Copyright 2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.FileFilter;
import java.util.Arrays;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.distributed.internal.DistributionConfig;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.core.env.PropertiesPropertySource;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.util.PropertiesBuilder;
import org.springframework.util.StringUtils;

/**
 * Integration Tests testing the configuration of Apache Geode Logging.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.distributed.internal.DistributionConfig
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext
 * @see org.springframework.core.env.PropertiesPropertySource
 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
 * @see org.springframework.data.gemfire.config.annotation.EnableLogging
 * @see org.springframework.data.gemfire.util.PropertiesBuilder
 * @since 1.0.0
 */
public class LoggingConfigurationIntegrationTests {

	private AtomicReference<Properties> propertiesReference = new AtomicReference<>(null);

	private ConfigurableApplicationContext applicationContext;

	@Before
	public void setup() {

		this.propertiesReference.set(null);

		deleteLogFiles();
	}

	@After
	public void tearDown() {

		Optional.ofNullable(this.applicationContext)
			.ifPresent(ConfigurableApplicationContext::close);

		deleteLogFiles();
	}

	private void assertGemFireCacheLogLevelAndLogFile(String logLevel, String logFile) {

		GemFireCache gemfireCache = this.applicationContext.getBean(GemFireCache.class);

		logFile = StringUtils.hasText(logFile) ? logFile : "";

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties distributedSystemProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(distributedSystemProperties).isNotNull();
		assertThat(distributedSystemProperties.getProperty(DistributionConfig.LOG_LEVEL_NAME)).isEqualTo(logLevel);
		assertThat(distributedSystemProperties.getProperty(DistributionConfig.LOG_FILE_NAME)).isEqualTo(logFile);
	}

	private void deleteLogFiles() {

		FileFilter gemfireTestLogFileFilter = file ->
			file.getName().startsWith("gemfire-logging-test") & file.getName().endsWith(".log");

		File[] files = new File(System.getProperty("user.dir")).listFiles(gemfireTestLogFileFilter);

		Arrays.stream(ArrayUtils.nullSafeArray(files, File.class)).forEach(File::delete);
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {

		AnnotationConfigApplicationContext applicationContext = new AnnotationConfigApplicationContext();

		applicationContext.register(annotatedClasses);
		applicationContext.registerShutdownHook();

		Optional.ofNullable(this.propertiesReference.get())
			.ifPresent(properties -> {
				applicationContext.getEnvironment().getPropertySources()
					.addFirst(new PropertiesPropertySource("Test Properties", properties));
			});

		applicationContext.refresh();

		this.applicationContext = applicationContext;

		return applicationContext;
	}

	private void with(Properties properties) {

		Optional.ofNullable(properties)
			.ifPresent(this.propertiesReference::set);
	}


	@Test
	public void clientCacheApplicationWithDefaultLoggingConfiguration() {

		newApplicationContext(ClientCacheApplicationWithDefaultLoggingTestConfiguration.class);

		assertGemFireCacheLogLevelAndLogFile("config", null);
	}

	@Test
	public void clientCacheApplicationWithLogLevelAttribute() {

		newApplicationContext(ClientCacheApplicationWithLogLevelTestConfiguration.class);

		assertGemFireCacheLogLevelAndLogFile("fine", null);
	}

	@Test
	public void clientCacheApplicationWithLogLevelProperty() {

		with(PropertiesBuilder.create().setProperty("spring.data.gemfire.cache.log-level", "info").build());

		newApplicationContext(ClientCacheApplicationWithLogLevelTestConfiguration.class);

		assertGemFireCacheLogLevelAndLogFile("info", null);

	}

	@Test
	public void withCustomLoggingEnabledClientCacheApplicationConfiguration() {

		newApplicationContext(ClientCacheApplicationWithDefaultLoggingTestConfiguration.class,
			CustomLoggingTestConfiguration.class);

		assertGemFireCacheLogLevelAndLogFile("warning", "gemfire-logging-test-zero.log");
	}

	@Test
	public void withCustomLoggingEnabledAndLogLevelPropertyConfiguredClientCacheApplicationConfiguration() {

		with(PropertiesBuilder.create()
			.setProperty("spring.data.gemfire.logging.level", "error")
			.setProperty("spring.data.gemfire.logging.log-file", "gemfire-logging-test-one.log")
			.build());

		newApplicationContext(ClientCacheApplicationWithLogLevelTestConfiguration.class,
			CustomLoggingTestConfiguration.class);

		assertGemFireCacheLogLevelAndLogFile("error", "gemfire-logging-test-one.log");
	}

	@Test
	public void withDefaultLoggingEnabledUsingClientCacheApplicationWithLogLevelConfiguration() {

		newApplicationContext(ClientCacheApplicationWithLogLevelTestConfiguration.class,
			DefaultLoggingTestConfiguration.class);

		assertGemFireCacheLogLevelAndLogFile("fine", null);
	}

	@Test
	public void withoutLoggingEnabledThenLoggingPropertiesHaveNoEffect() {

		with(PropertiesBuilder.create()
			.setProperty("spring.data.gemfire.logging.level", "error")
			.setProperty("spring.data.gemfire.logging.log-file", "gemfire-logging-test-two.log")
			.build());

		newApplicationContext(ClientCacheApplicationWithLogLevelTestConfiguration.class);

		assertGemFireCacheLogLevelAndLogFile("fine", null);
	}

	@ClientCacheApplication(name = "ClientCacheApplicationWithDefaultLogging")
	static class ClientCacheApplicationWithDefaultLoggingTestConfiguration { }

	@ClientCacheApplication(name = "ClientCacheApplicationWithLogLevel", logLevel = "fine")
	static class ClientCacheApplicationWithLogLevelTestConfiguration { }

	@EnableLogging
	static class DefaultLoggingTestConfiguration { }

	@EnableLogging(logLevel = "warning", logFile = "gemfire-logging-test-zero.log")
	static class CustomLoggingTestConfiguration { }

}
