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
package org.springframework.data.gemfire;

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.StreamSupport;

import org.apache.geode.distributed.Locator;
import org.apache.geode.distributed.LocatorLauncher;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.data.gemfire.config.annotation.LocatorConfigurer;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} used to configure and initialize (bootstrap) an Apache Geode or Pivotal GemFire
 * {@link Locator} using the {@link LocatorLauncher} class.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.apache.geode.distributed.Locator
 * @see org.apache.geode.distributed.LocatorLauncher
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.data.gemfire.config.annotation.LocatorConfigurer
 * @see org.springframework.data.gemfire.support.AbstractFactoryBeanSupport
 * @since 2.2.0
 */
@SuppressWarnings("unused")
public class LocatorFactoryBean extends AbstractFactoryBeanSupport<Locator> implements InitializingBean {

	public static final int DEFAULT_PORT = 10334;

	public static final String DEFAULT_LOG_LEVEL = "config";
	public static final String LOG_LEVEL_PROPERTY = "log-level";

	private Integer port = DEFAULT_PORT;

	private List<LocatorConfigurer> locatorConfigurers = new ArrayList<>();

	private Locator locator;

	private LocatorConfigurer compositeLocatorConfigurer = (beanName, bean) ->
		nullSafeList(this.locatorConfigurers).forEach(locatorConfigurer ->
			locatorConfigurer.configure(beanName, bean));

	private LocatorLauncher locatorLauncher;

	private Properties gemfireProperties;

	private String bindAddress;
	private String hostnameForClients;
	private String logLevel;
	private String name;

	@Override
	public void afterPropertiesSet() throws Exception {
		applyLocatorConfigurers(getCompositeLocatorConfigurer());
		init();
	}

	protected void applyLocatorConfigurers(LocatorConfigurer... locatorConfigurers) {
		applyLocatorConfigurers(Arrays.asList(nullSafeArray(locatorConfigurers, LocatorConfigurer.class)));
	}

	protected void applyLocatorConfigurers(Iterable<LocatorConfigurer> locatorConfigurers) {
		StreamSupport.stream(nullSafeIterable(locatorConfigurers).spliterator(), false)
			.forEach(locatorConfigurer -> locatorConfigurer.configure(getBeanName(), this));
	}

	public void init() {

		LocatorLauncher.Builder locatorBuilder = configureGemfireProperties(newLocatorLauncherBuilder());

		getBindAddress().ifPresent(locatorBuilder::setBindAddress);
		getHostnameForClients().ifPresent(locatorBuilder::setHostnameForClients);
		getName().ifPresent(locatorBuilder::setMemberName);

		locatorBuilder.set(LOG_LEVEL_PROPERTY, getLogLevel());
		locatorBuilder.setPort(getPort());

		locatorBuilder = postProcess(locatorBuilder);

		this.locatorLauncher = postProcess(locatorBuilder.build());

		LocatorLauncher.LocatorState locatorState = this.locatorLauncher.start();

		/*
		if (LocatorLauncher.Status.ONLINE.equals(locatorState.getStatus())) {
			// log warning
		}
		*/

		this.locator = this.locatorLauncher.getLocator();
	}

	protected LocatorLauncher.Builder configureGemfireProperties(LocatorLauncher.Builder locatorBuilder) {

		Properties gemfireProperties = getGemFireProperties();

		gemfireProperties.stringPropertyNames().stream()
			.forEach(propertyName -> locatorBuilder.set(propertyName, gemfireProperties.getProperty(propertyName)));

		return locatorBuilder;
	}

	protected LocatorLauncher.Builder newLocatorLauncherBuilder() {
		return new LocatorLauncher.Builder();
	}

	protected LocatorLauncher.Builder postProcess(LocatorLauncher.Builder locatorBuilder) {
		return locatorBuilder;
	}

	protected LocatorLauncher postProcess(LocatorLauncher locatorLauncher) {
		return locatorLauncher;
	}

	public Locator getLocator() {
		return this.locator;
	}

	public LocatorLauncher getLocatorLauncher() {
		return this.locatorLauncher;
	}

	@Nullable @Override
	public Locator getObject() throws Exception {

		Locator locator = getLocator();

		Assert.state(locator != null, "Locator was not configured and initialized");

		return locator;
	}

	@Nullable @Override
	public Class<?> getObjectType() {

		Locator locator  = getLocator();

		return locator != null ? locator.getClass() : Locator.class;
	}

	public void setBindAddress(String bindAddress) {
		this.bindAddress = bindAddress;
	}

	public Optional<String> getBindAddress() {

		return Optional.ofNullable(this.bindAddress)
			.filter(StringUtils::hasText);
	}

	public LocatorConfigurer getCompositeLocatorConfigurer() {
		return this.compositeLocatorConfigurer;
	}

	public void setGemFireProperties(Properties gemfireProperties) {
		this.gemfireProperties = gemfireProperties;
	}

	public Properties getGemFireProperties() {

		if (this.gemfireProperties == null) {
			this.gemfireProperties = new Properties();
		}

		return this.gemfireProperties;
	}

	public void setHostnameForClients(String hostnameForClients) {
		this.hostnameForClients = hostnameForClients;
	}

	public Optional<String> getHostnameForClients() {

		return Optional.ofNullable(this.hostnameForClients)
			.filter(StringUtils::hasText);
	}

	public void setLocatorConfigurers(LocatorConfigurer... locatorConfigurers) {
		setLocatorConfigurers(Arrays.asList(nullSafeArray(locatorConfigurers, LocatorConfigurer.class)));
	}

	public void setLocatorConfigurers(List<LocatorConfigurer> locatorConfigurers) {
		Optional.ofNullable(locatorConfigurers).ifPresent(this.locatorConfigurers::addAll);
	}

	public void setLogLevel(String logLevel) {
		this.logLevel = logLevel;
	}

	public String getLogLevel() {
		return StringUtils.hasText(this.logLevel) ? this.logLevel : DEFAULT_LOG_LEVEL;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Optional<String> getName() {

		return Optional.ofNullable(this.name)
			.filter(StringUtils::hasText);
	}

	public void setPort(Integer port) {

		Assert.isTrue(port >= 0 && port < 65536, String.format("Network port [%d] is not valid", port));

		this.port = port;
	}

	public Integer getPort() {
		return this.port;
	}
}
