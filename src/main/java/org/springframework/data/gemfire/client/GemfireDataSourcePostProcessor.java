/*
 * Copyright 2002-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.client;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionFactory;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.management.internal.cli.domain.RegionInformation;
import org.apache.geode.management.internal.cli.functions.GetRegionsFunction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.BeansException;
import org.springframework.beans.TypeMismatchException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.data.gemfire.client.function.ListRegionsOnServerFunction;
import org.springframework.data.gemfire.function.execution.GemfireOnServersFunctionTemplate;
import org.springframework.lang.Nullable;
import org.springframework.util.ObjectUtils;

/**
 * A Spring {@link BeanFactoryPostProcessor} used to register a Client Region beans for each Region accessible to
 * an Apache Geode or Pivotal GemFire DataSource. If the Region is already defined, the bean definition
 * will not be overridden.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientRegionFactory
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @see org.apache.geode.cache.execute.Function
 * @see org.apache.geode.management.internal.cli.functions.GetRegionsFunction
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @see org.springframework.data.gemfire.client.function.ListRegionsOnServerFunction
 * @see org.springframework.data.gemfire.function.execution.GemfireOnServersFunctionTemplate
 * @see ListRegionsOnServerFunction
 * @since 1.2.0
 */
public class GemfireDataSourcePostProcessor implements BeanFactoryAware, BeanPostProcessor {

	private static final ClientRegionShortcut DEFAULT_CLIENT_REGION_SHORTCUT = ClientRegionShortcut.PROXY;

	private ClientRegionShortcut clientRegionShortcut;

	private ConfigurableBeanFactory beanFactory;

	private final Logger logger = LoggerFactory.getLogger(getClass());

	/**
	 * Set a reference to the {@link BeanFactory}.
	 *
	 * @param beanFactory reference to the {@link BeanFactory}.
	 * @throws BeansException if the {@link BeanFactory} is not a {@link ConfigurableBeanFactory}.
	 * @see org.springframework.beans.factory.config.ConfigurableBeanFactory
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {

		if (beanFactory instanceof ConfigurableBeanFactory) {
			this.beanFactory = (ConfigurableBeanFactory) beanFactory;
		}
		else {
			throw new TypeMismatchException(beanFactory, ConfigurableBeanFactory.class);
		}
	}

	/**
	 * Returns a reference to the configured {@link ConfigurableBeanFactory}.
	 *
	 * @return a reference to the configured {@link ConfigurableBeanFactory}.
	 * @see org.springframework.beans.factory.config.ConfigurableBeanFactory
	 */
	public Optional<ConfigurableBeanFactory> getBeanFactory() {
		return Optional.ofNullable(this.beanFactory);
	}

	/**
	 * Set the data policy used to configure the client {@link Region}.
	 *
	 * @param clientRegionShortcut {@link ClientRegionShortcut} used to define the data policy
	 * used by the client {@link Region}.
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 */
	public void setClientRegionShortcut(ClientRegionShortcut clientRegionShortcut) {
		this.clientRegionShortcut = clientRegionShortcut;
	}

	/**
	 * Returns the data policy used to configure the client {@link Region}.
	 *
	 * @return the configured {@link ClientRegionShortcut} used to define the data policy
	 * used by the client {@link Region}.
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 * @see java.util.Optional
	 */
	public Optional<ClientRegionShortcut> getClientRegionShortcut() {
		return Optional.ofNullable(this.clientRegionShortcut);
	}

	/**
	 * Resolves the {@link ClientRegionShortcut} used to configure and create client {@link Region Regions}.
	 *
	 * @return the resolved {@link ClientRegionShortcut}.
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 * @see #getClientRegionShortcut()
	 */
	protected ClientRegionShortcut resolveClientRegionShortcut() {
		return getClientRegionShortcut().orElse(DEFAULT_CLIENT_REGION_SHORTCUT);
	}

	/**
	 * Returns a reference to the configured {@link Logger} used to log messages.
	 *
	 * @return a reference to the configured {@link Logger}.
	 * @see org.slf4j.Logger
	 */
	protected Logger getLogger() {
		return this.logger;
	}

	@Nullable @Override
	public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {

		if (bean instanceof ClientCache) {

			ClientCache clientCache = (ClientCache) bean;

			getBeanFactory().ifPresent(it -> createClientProxyRegions(it, clientCache, regionNames(clientCache)));
		}

		return bean;
	}


	// TODO: remove this logic and delegate to o.s.d.g.config.remote.GemfireAdminOperations
	Iterable<String> regionNames(ClientCache clientCache) {

		try {
			return execute(clientCache, new ListRegionsOnServerFunction());
		}
		catch (Exception ignore) {

			try {

				Object results = execute(clientCache, new GetRegionsFunction());

				List<String> regionNames = Collections.emptyList();

				if (containsRegionInformation(results)) {

					Object[] resultsArray = (Object[]) results;

					regionNames = new ArrayList<>(resultsArray.length);

					for (Object result : resultsArray) {
						regionNames.add(((RegionInformation) result).getName());
					}
				}

				return regionNames;
			}
			catch (Exception cause) {
				logDebug("Failed to determine the Regions available on the Server: %n%s", cause);
				return Collections.emptyList();
			}
		}
	}

	<T> T execute(ClientCache clientCache, Function gemfireFunction, Object... arguments) {
		return new GemfireOnServersFunctionTemplate(clientCache).executeAndExtract(gemfireFunction, arguments);
	}

	boolean containsRegionInformation(Object results) {

		return results instanceof Object[] && ((Object[]) results).length > 0
			&& ((Object[]) results)[0] instanceof RegionInformation;
	}

	void createClientProxyRegions(ConfigurableBeanFactory beanFactory, ClientCache clientCache,
			Iterable<String> regionNames) {

		if (regionNames.iterator().hasNext()) {

			ClientRegionShortcut resolvedClientRegionShortcut = resolveClientRegionShortcut();

			ClientRegionFactory<?, ?> clientRegionFactory =
				clientCache.createClientRegionFactory(resolvedClientRegionShortcut);

			for (String regionName : regionNames) {

				if (beanFactory.containsBean(regionName)) {

					Object bean = beanFactory.getBean(regionName);

					logWarn("Cannot create a client {} Region bean named {}; A bean with name {} having type {} already exists",
						resolvedClientRegionShortcut.name(), regionName, regionName, ObjectUtils.nullSafeClassName(bean));
				}
				else {
					logInfo("Creating Region bean with name {}...", regionName);
					beanFactory.registerSingleton(regionName, clientRegionFactory.create(regionName));
				}
			}
		}
	}

	void logDebug(String message, Object... arguments) {

		Logger logger = getLogger();

		if (logger.isDebugEnabled()) {
			logger.debug(String.format(message, arguments));
		}
	}

	void logInfo(String message, Object... arguments) {

		Logger logger = getLogger();

		if (logger.isInfoEnabled()) {
			logger.info(message, arguments);
		}
	}

	void logWarn(String message, Object... arguments) {

		Logger logger = getLogger();

		if (logger.isWarnEnabled()) {
			logger.warn(message, arguments);
		}
	}

	public GemfireDataSourcePostProcessor using(ClientRegionShortcut clientRegionShortcut) {

		setClientRegionShortcut(clientRegionShortcut);

		return this;
	}

	public GemfireDataSourcePostProcessor using(BeanFactory beanFactory) {

		setBeanFactory(beanFactory);

		return this;
	}
}
