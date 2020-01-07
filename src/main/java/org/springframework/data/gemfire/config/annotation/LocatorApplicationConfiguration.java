/*
 * Copyright 2018-2020 the original author or authors.
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

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.distributed.Locator;

import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.LocatorFactoryBean;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.util.ClassUtils;

/**
 * Spring {@link Configuration @Configuration} class used to configure and bootstrap an Apache Geode
 * or Pivotal GemFire {@link Locator}.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.apache.geode.distributed.Locator
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see org.springframework.core.annotation.AnnotationAttributes
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.LocatorFactoryBean
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.LocatorConfigurer
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @since 2.2.0
 */
@Configuration
@SuppressWarnings("unused")
public class LocatorApplicationConfiguration extends AbstractAnnotationConfigSupport implements ImportAware {

	public static final int DEFAULT_PORT = 10334;

	public static final String DEFAULT_LOG_LEVEL = "config";
	public static final String DEFAULT_NAME = "SpringBasedLocatorApplication";

	protected static final String EXCLUSIVE_LOCATOR_APPLICATION_ERROR_MESSAGE =
		"A Spring application cannot be both a Cache and a Locator application;"
			+ " You may annotate your Spring application main class with either 1 of"
			+ " [@ClientCacheApplication, @CacheServerApplication, @PeerCacheApplication] or @LocatorApplication;"
			+ " If you want to create a Spring, Apache Geode/Pivotal GemFire server application "
			+ " (i.e. [@PeerCacheApplication,  @CacheServerApplication] and also run an embedded Locator service,"
			+ " then use @EnableLocator with 1 of the server-side, cache application annotations instead;"
			+ " Locators are not applicable to clients.";

	private static final List<String> CACHE_FACTORY_BEAN_CLASS_NAMES =
		Arrays.asList(CacheFactoryBean.class.getName(), ClientCacheFactoryBean.class.getName());

	private int port = DEFAULT_PORT;

	@Autowired(required = false)
	private List<LocatorConfigurer> locatorConfigurers = Collections.emptyList();

	private String bindAddress;
	private String hostnameForClients;
	private String logLevel;
	private String name;

	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return LocatorApplication.class;
	}

	@Bean
	BeanFactoryPostProcessor exclusiveLocatorApplicationBeanFactoryPostProcessor() {

		return configurableListableBeanFactory -> {

			String[] beanDefinitionNames = configurableListableBeanFactory.getBeanDefinitionNames();

			boolean match = Arrays.stream(nullSafeArray(beanDefinitionNames, String.class))
				.map(configurableListableBeanFactory::getBeanDefinition)
				.anyMatch(beanDefinition ->

					resolveBeanClassName(beanDefinition)
						.map(beanClassName -> {
							try {

								Class<?> possibleCacheType =
									ClassUtils.resolveClassName(beanClassName, getBeanClassLoader());

								return isCacheType(possibleCacheType);
							}
							catch (Throwable ignore) {
								return CACHE_FACTORY_BEAN_CLASS_NAMES.contains(beanClassName);
							}
						})
						.orElse(false));

			if (match) {
				throw new BeanDefinitionStoreException(EXCLUSIVE_LOCATOR_APPLICATION_ERROR_MESSAGE);
			}
		};
	}

	private boolean isCacheType(Class<?> type) {

		return type != null
			&& (CacheFactoryBean.class.isAssignableFrom(type) || GemFireCache.class.isAssignableFrom(type));
	}

	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {

		if (isAnnotationPresent(importMetadata)) {

			AnnotationAttributes locatorApplicationAnnotationAttributes = getAnnotationAttributes(importMetadata);

			setBindAddress(resolveProperty(locatorProperty("bind-address"),
				locatorApplicationAnnotationAttributes.getString("bindAddress")));

			setHostnameForClients(resolveProperty(locatorProperty("hostname-for-clients"),
				locatorApplicationAnnotationAttributes.getString("hostnameForClients")));

			setLogLevel(resolveProperty(locatorProperty("log-level"),
				locatorApplicationAnnotationAttributes.getString("logLevel")));

			setName(resolveProperty(locatorProperty("name"),
				locatorApplicationAnnotationAttributes.getString("name")));

			setPort(resolveProperty(locatorProperty("port"),
				locatorApplicationAnnotationAttributes.<Integer>getNumber("port")));
		}
	}

	@Bean
	public LocatorFactoryBean locatorApplication() {

		LocatorFactoryBean locatorFactoryBean = new LocatorFactoryBean();

		locatorFactoryBean.setBindAddress(getBindAddress());
		locatorFactoryBean.setHostnameForClients(getHostnameForClients());
		locatorFactoryBean.setLocatorConfigurers(resolveLocatorConfigurers());
		locatorFactoryBean.setLogLevel(getLogLevel());
		locatorFactoryBean.setName(getName());
		locatorFactoryBean.setPort(getPort());

		return locatorFactoryBean;
	}

	private List<LocatorConfigurer> resolveLocatorConfigurers() {

		return Optional.ofNullable(this.locatorConfigurers)
			.filter(locatorConfigurers -> !locatorConfigurers.isEmpty())
			.orElseGet(() ->
				Collections.singletonList(LazyResolvingComposableLocatorConfigurer.create(getBeanFactory())));
	}

	public void setBindAddress(String bindAddress) {
		this.bindAddress = bindAddress;
	}

	public String getBindAddress() {
		return this.bindAddress;
	}

	public void setHostnameForClients(String hostnameForClients) {
		this.hostnameForClients = hostnameForClients;
	}

	public String getHostnameForClients() {
		return this.hostnameForClients;
	}

	public void setLogLevel(String logLevel) {
		this.logLevel = logLevel;
	}

	public String getLogLevel() {
		return this.logLevel;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return this.name;
	}

	public void setPort(int port) {
		this.port = port;
	}

	public int getPort() {
		return this.port;
	}
}
