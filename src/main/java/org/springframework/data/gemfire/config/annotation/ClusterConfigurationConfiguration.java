/*
 * Copyright 2017-2020 the original author or authors.
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

import static org.springframework.data.gemfire.util.CacheUtils.isClient;
import static org.springframework.data.gemfire.util.CacheUtils.isPeer;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeMap;

import java.lang.annotation.Annotation;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.query.Index;

import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.core.OrderComparator;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.env.Environment;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.config.admin.remote.FunctionGemfireAdminTemplate;
import org.springframework.data.gemfire.config.admin.remote.RestHttpGemfireAdminTemplate;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.config.schema.SchemaObjectCollector;
import org.springframework.data.gemfire.config.schema.SchemaObjectDefiner;
import org.springframework.data.gemfire.config.schema.definitions.RegionDefinition;
import org.springframework.data.gemfire.config.schema.support.ClientRegionCollector;
import org.springframework.data.gemfire.config.schema.support.ComposableSchemaObjectCollector;
import org.springframework.data.gemfire.config.schema.support.ComposableSchemaObjectDefiner;
import org.springframework.data.gemfire.config.schema.support.IndexCollector;
import org.springframework.data.gemfire.config.schema.support.IndexDefiner;
import org.springframework.data.gemfire.config.schema.support.RegionDefiner;
import org.springframework.data.gemfire.config.support.AbstractSmartLifecycle;
import org.springframework.data.gemfire.config.support.RestTemplateConfigurer;
import org.springframework.data.gemfire.util.CacheUtils;
import org.springframework.data.gemfire.util.NetworkUtils;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Spring {@link Configuration @Configuration} class defining Spring beans that will record the creation of
 * Apache Geode / Pivotal GemFire {@link Region Regions} defined in Spring config (i.e. XML, Java or by Annotations)
 * as Spring beans in the Spring container.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.beans.factory.ListableBeanFactory
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see org.springframework.context.event.EventListener
 * @see org.springframework.core.annotation.AnnotationAttributes
 * @see org.springframework.core.env.Environment
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @see org.springframework.http.client.ClientHttpRequestInterceptor
 * @since 2.0.0
 */
@Configuration
@SuppressWarnings("unused")
public class ClusterConfigurationConfiguration extends AbstractAnnotationConfigSupport implements ImportAware {

	protected static final boolean DEFAULT_HTTP_FOLLOW_REDIRECTS = false;
	protected static final boolean DEFAULT_HTTP_REQUEST_INTERCEPTORS_ENABLED = false;
	protected static final boolean DEFAULT_MANAGEMENT_USE_HTTP = false;
	protected static final boolean DEFAULT_MANAGEMENT_REQUIRE_HTTPS = true;

	protected static final int DEFAULT_MANAGEMENT_HTTP_PORT = HttpServiceConfiguration.DEFAULT_HTTP_SERVICE_PORT;

	protected static final String DEFAULT_MANAGEMENT_HTTP_HOST = "localhost";
	protected static final String HTTP_FOLLOW_REDIRECTS_PROPERTY = "spring.data.gemfire.management.http.follow-redirects";
	protected static final String HTTP_SCHEME = "http";
	protected static final String HTTPS_SCHEME = "https";

	private static final RegionShortcut DEFAULT_SERVER_REGION_SHORTCUT = RegionDefinition.DEFAULT_REGION_SHORTCUT;

	private Boolean enableInterceptors = DEFAULT_HTTP_REQUEST_INTERCEPTORS_ENABLED;
	private Boolean followRedirects = DEFAULT_HTTP_FOLLOW_REDIRECTS;
	private Boolean requireHttps = DEFAULT_MANAGEMENT_REQUIRE_HTTPS;
	private Boolean useHttp = DEFAULT_MANAGEMENT_USE_HTTP;

	private Integer managementHttpPort = DEFAULT_MANAGEMENT_HTTP_PORT;

	@Autowired(required = false)
	private GemfireAdminOperations gemfireAdminOperations;

	@Autowired(required = false)
	private List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors;

	@Autowired(required = false)
	private List<RestTemplateConfigurer> restTemplateConfigurers;

	private RegionShortcut serverRegionShortcut;

	private String managementHttpHost = DEFAULT_MANAGEMENT_HTTP_HOST;

	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableClusterConfiguration.class;
	}

	protected void setManagementHttpHost(String hostname) {
		this.managementHttpHost = hostname;
	}

	protected Optional<String> getManagementHttpHost() {
		return Optional.ofNullable(this.managementHttpHost).filter(StringUtils::hasText);
	}

	protected String resolveManagementHttpHost() {
		return getManagementHttpHost().orElse(DEFAULT_MANAGEMENT_HTTP_HOST);
	}

	protected void setManagementHttpPort(Integer managementHttpPort) {
		this.managementHttpPort = managementHttpPort;
	}

	protected Optional<Integer> getManagementHttpPort() {
		return Optional.ofNullable(this.managementHttpPort);
	}

	protected int resolveManagementHttpPort() {
		return getManagementHttpPort().orElse(DEFAULT_MANAGEMENT_HTTP_PORT);
	}

	protected void setManagementHttpEnableInterceptors(Boolean enableInterceptors) {
		this.enableInterceptors = enableInterceptors;
	}

	protected Optional<Boolean> getManagementHttpEnableInterceptors() {
		return Optional.ofNullable(this.enableInterceptors);
	}

	protected boolean resolveManagementHttpEnableInterceptors() {
		return getManagementHttpEnableInterceptors().orElse(DEFAULT_HTTP_REQUEST_INTERCEPTORS_ENABLED);
	}

	protected void setManagementHttpFollowRedirects(Boolean followRedirects) {
		this.followRedirects = followRedirects;
	}

	protected Optional<Boolean> getManagementHttpFollowRedirects() {
		return Optional.ofNullable(this.followRedirects);
	}

	protected boolean resolveManagementHttpFollowRedirects() {
		return getManagementHttpFollowRedirects().orElse(DEFAULT_HTTP_FOLLOW_REDIRECTS);
	}

	protected void setManagementRequireHttps(Boolean requireHttps) {
		this.requireHttps = requireHttps;
	}

	protected Optional<Boolean> getManagementRequireHttps() {
		return Optional.ofNullable(this.requireHttps);
	}

	protected boolean resolveManagementRequireHttps() {
		return getManagementRequireHttps().orElse(DEFAULT_MANAGEMENT_REQUIRE_HTTPS);
	}

	protected void setManagementUseHttp(Boolean useHttp) {
		this.useHttp = useHttp;
	}

	protected Optional<Boolean> getManagementUseHttp() {
		return Optional.ofNullable(this.useHttp);
	}

	protected boolean resolveManagementUseHttp() {
		return getManagementUseHttp().orElse(DEFAULT_MANAGEMENT_USE_HTTP);
	}

	protected void setServerRegionShortcut(RegionShortcut regionShortcut) {
		this.serverRegionShortcut = regionShortcut;
	}

	protected Optional<RegionShortcut> getServerRegionShortcut() {
		return Optional.ofNullable(this.serverRegionShortcut);
	}

	protected RegionShortcut resolveServerRegionShortcut() {
		return getServerRegionShortcut().orElse(DEFAULT_SERVER_REGION_SHORTCUT);
	}

	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {

		if (isAnnotationPresent(importMetadata)) {

			AnnotationAttributes enableClusterConfigurationAttributes = getAnnotationAttributes(importMetadata);

			setManagementHttpHost(resolveProperty(managementProperty("http.host"),
				enableClusterConfigurationAttributes.getString("host")));

			setManagementHttpPort(resolveProperty(managementProperty("http.port"),
				enableClusterConfigurationAttributes.<Integer>getNumber("port")));

			setManagementHttpEnableInterceptors(resolveProperty(managementProperty("http.enable-interceptors"),
				enableClusterConfigurationAttributes.getBoolean("enableInterceptors")));

			setManagementHttpFollowRedirects(resolveProperty(managementProperty("http.follow-redirects"),
				enableClusterConfigurationAttributes.getBoolean("followRedirects")));

			setManagementRequireHttps(resolveProperty(managementProperty("require-https"),
				enableClusterConfigurationAttributes.getBoolean("requireHttps")));

			setManagementUseHttp(resolveProperty(managementProperty("use-http"),
				enableClusterConfigurationAttributes.getBoolean("useHttp")));

			setServerRegionShortcut(resolveProperty(clusterProperty("region.type"),
				RegionShortcut.class, enableClusterConfigurationAttributes.getEnum("serverRegionShortcut")));
		}
	}

	@Bean
	public ClusterSchemaObjectInitializer gemfireClusterSchemaObjectInitializer(Environment environment,
			GemFireCache gemfireCache) {

		return Optional.ofNullable(gemfireCache)
			.filter(CacheUtils::isClient)
			.map(clientCache -> {

				SchemaObjectContext schemaObjectContext = SchemaObjectContext.from(gemfireCache)
					.with(resolveGemfireAdminOperations(environment, (ClientCache) clientCache))
					.with(newSchemaObjectCollector())
					.with(newSchemaObjectDefiner());

				return new ClusterSchemaObjectInitializer(schemaObjectContext);

			})
			.orElse(null);
	}

	private <T> List<T> resolveBeansOfType(List<T> objects, Class<T> type) {

		return Optional.ofNullable(objects).orElseGet(() ->
			Optional.of(getBeanFactory())
				.filter(ListableBeanFactory.class::isInstance)
				.map(ListableBeanFactory.class::cast)
				.map(beanFactory -> {

					Map<String, T> beansOfType = beanFactory.getBeansOfType(type, true, false);

					return nullSafeMap(beansOfType).values().stream().collect(Collectors.toList());

				})
				.orElseGet(Collections::emptyList));
	}

	/**
	 * Attempts to resolve a {@link List} of {@link ClientHttpRequestInterceptor} beans in the Spring
	 * {@link ApplicationContext}.
	 *
	 * @return a {@link List} of declared and registered {@link ClientHttpRequestInterceptor} beans.
	 * @see org.springframework.http.client.ClientHttpRequestInterceptor
	 * @see java.util.List
	 */
	protected List<ClientHttpRequestInterceptor> resolveClientHttpRequestInterceptors(boolean enableInterceptors) {

		return enableInterceptors
			? resolveBeansOfType(this.clientHttpRequestInterceptors, ClientHttpRequestInterceptor.class)
			: Collections.emptyList();
	}

	/**
	 * Attempts to resolve a {@link List} of {@link RestTemplateConfigurer} beans in the Spring
	 * {@link ApplicationContext}.
	 *
	 * @return a {@link List} of declared and registered {@link RestTemplateConfigurer} beans.
	 * @see org.springframework.data.gemfire.config.support.RestTemplateConfigurer
	 * @see java.util.List
	 */
	protected List<RestTemplateConfigurer> resolveRestTemplateConfigurers() {
		return resolveBeansOfType(this.restTemplateConfigurers, RestTemplateConfigurer.class);
	}

	/**
	 * Attempts to resolve the the {@link GemfireAdminOperations} object from the Spring {@link ApplicationContext}
	 * which is used to create Apache Geode or Pivotal GemFire schema objects.
	 *
	 * @param environment reference to the {@link Environment}.
	 * @param clientCache reference to the {@link ClientCache}.
	 * @return the resovled {@link GemfireAdminOperations} instance.
	 * @see org.springframework.core.env.Environment
	 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
	 * @see org.apache.geode.cache.client.ClientCache
	 * @see #newGemfireAdminOperations(Environment, ClientCache)
	 */
	protected GemfireAdminOperations resolveGemfireAdminOperations(Environment environment, ClientCache clientCache) {

		return Optional.ofNullable(this.gemfireAdminOperations)
			.orElseGet(() -> newGemfireAdminOperations(environment, clientCache));
	}


	/**
	 * Constructs a new instance of {@link GemfireAdminOperations} to perform administrative, schema functions
	 * on a GemFire cache cluster as well as a client cache from a cache client.
>>>>>>> d4f8a960... DATAGEODE-192 - Add support for HTTPS and Follow Redirects when using @EnableClusterConfiguration.
	 *
	 * @param environment reference to the {@link Environment}.
	 * @param clientCache {@link ClientCache} instance used by the {@link GemfireAdminOperations} interface
	 * to access the Pivotal GemFire system.
	 * @return an implementation of the {@link GemfireAdminOperations} interface to perform administrative functions
	 * on a Pivotal GemFire system.
	 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
	 * @see org.apache.geode.cache.client.ClientCache
	 * @see #resolveClientHttpRequestInterceptors(boolean)
	 * @see #resolveManagementHttpHost()
	 * @see #resolveManagementHttpPort()
	 * @see #resolveManagementRequireHttps()
	 * @see #resolveManagementUseHttp()
	 */
	private GemfireAdminOperations newGemfireAdminOperations(Environment environment, ClientCache clientCache) {

		if (resolveManagementUseHttp()) {

			boolean enableInterceptors = resolveManagementHttpEnableInterceptors();
			boolean followRedirects = resolveManagementHttpFollowRedirects();
			boolean requireHttps = resolveManagementRequireHttps();
			boolean resolvedFollowRedirects = !requireHttps || followRedirects;

			int port = resolveManagementHttpPort();

			String host = resolveManagementHttpHost();
			String scheme = requireHttps ? HTTPS_SCHEME : HTTP_SCHEME;

			return configurePort(new RestHttpGemfireAdminTemplate.Builder()
				.withConfigurers(resolveRestTemplateConfigurers())
				.withInterceptors(resolveClientHttpRequestInterceptors(enableInterceptors))
				.with(clientCache)
				.using(scheme)
				.on(host)
				.followRedirects(resolvedFollowRedirects), port)
				.build();
		}
		else {
			return new FunctionGemfireAdminTemplate(clientCache);
		}
	}

	private RestHttpGemfireAdminTemplate.Builder configurePort(RestHttpGemfireAdminTemplate.Builder builder, int port) {

		return NetworkUtils.isValidNonEphemeralPort(port)
			? builder.listenOn(port)
			: builder;
	}

	/**
	 * Constructs a new instance of {@link SchemaObjectCollector} to inspect the application's context
	 * and find all the GemFire schema objects declared of a particular type or types.
	 *
	 * @return a new instance of {@link SchemaObjectCollector} to inspect a Pivotal GemFire system schema
	 * in search of specific Pivotal GemFire schema objects (e.g. {@link Region} or {@link Index}).
	 * @see org.springframework.data.gemfire.config.schema.SchemaObjectCollector
	 */
	private SchemaObjectCollector<?> newSchemaObjectCollector() {

		return ComposableSchemaObjectCollector.compose(
			new ClientRegionCollector(),
			new IndexCollector()
		);
	}

	/**
	 * Constructs a new instance of {@link SchemaObjectDefiner} used to reverse engineer a GemFire schema object instance
	 * to build a definition.
	 *
	 * @return a new instance of {@link SchemaObjectDefiner}.
	 * @see org.springframework.data.gemfire.config.schema.SchemaObjectDefiner
	 */
	private SchemaObjectDefiner newSchemaObjectDefiner() {

		return ComposableSchemaObjectDefiner.compose(
			new RegionDefiner(resolveServerRegionShortcut()),
			new IndexDefiner()
		);
	}

	public static class ClusterSchemaObjectInitializer extends AbstractSmartLifecycle {

		private final SchemaObjectContext schemaObjectContext;

		protected ClusterSchemaObjectInitializer(SchemaObjectContext schemaObjectContext) {

			Assert.notNull(schemaObjectContext, "SchemaObjectContext is required");

			this.schemaObjectContext = schemaObjectContext;
		}

		@Override
		public boolean isAutoStartup() {
			return true;
		}

		@Override
		public int getPhase() {
			return Integer.MIN_VALUE;
		}

		public SchemaObjectContext getSchemaObjectContext() {
			return this.schemaObjectContext;
		}

		@Override
		public void start() {

			SchemaObjectContext schemaObjectContext = getSchemaObjectContext();

			if (schemaObjectContext.isClientCache()) {

				Iterable<?> schemaObjects = schemaObjectContext.getSchemaObjectCollector()
					.collectFrom(requireApplicationContext());

				//Iterable<?> cacheSchemaObjects = schemaObjectContext.getSchemaObjectCollector()
				//	.collectFrom(schemaObjectContext.<GemFireCache>getGemfireCache());

				StreamSupport.stream(schemaObjects.spliterator(), false)
					.map(schemaObjectContext.getSchemaObjectDefiner()::define)
					.sorted(OrderComparator.INSTANCE)
					.forEach(schemaObjectDefinition -> schemaObjectDefinition.ifPresent(it ->
						it.create(schemaObjectContext.getGemfireAdminOperations())));

				setRunning(true);
			}
			/*
			else if (schemaObjectContext.isPeerCache()) {

				GemfireFunctionUtils.registerFunctionForPojoMethod(new CreateRegionFunction(),
					CreateRegionFunction.CREATE_REGION_FUNCTION_ID);

				GemfireFunctionUtils.registerFunctionForPojoMethod(new CreateIndexFunction(),
					CreateIndexFunction.CREATE_INDEX_FUNCTION_ID);
			}
			*/
		}

		@Override
		public void stop() {
			setRunning(false);
		}

		@Override
		public void stop(Runnable callback) {
			setRunning(false);
			callback.run();
		}
	}

	public static class SchemaObjectContext {

		private final GemFireCache gemfireCache;

		private GemfireAdminOperations gemfireAdminOperations;

		private SchemaObjectCollector<?> schemaObjectCollector;

		private SchemaObjectDefiner schemaObjectDefiner;

		protected static SchemaObjectContext from(GemFireCache gemfireCache) {
			return new SchemaObjectContext(gemfireCache);
		}

		private SchemaObjectContext(GemFireCache gemfireCache) {
			Assert.notNull(gemfireCache, "GemFireCache is required");
			this.gemfireCache = gemfireCache;
		}

		@SuppressWarnings("unchecked")
		public <T extends GemfireAdminOperations> T getGemfireAdminOperations() {

			Assert.state(this.gemfireAdminOperations != null,
				"GemfireAdminOperations was not initialized");

			return (T) this.gemfireAdminOperations;
		}

		public boolean isClientCache() {
			return isClient(getGemfireCache());
		}

		public boolean isPeerCache() {
			return isPeer(getGemfireCache());
		}

		@SuppressWarnings("unchecked")
		public <T extends GemFireCache> T getGemfireCache() {
			return (T) this.gemfireCache;
		}

		public SchemaObjectCollector<?> getSchemaObjectCollector() {

			Assert.state(this.schemaObjectCollector != null,
				"SchemaObjectCollector was not initialized");

			return this.schemaObjectCollector;
		}


		public SchemaObjectDefiner getSchemaObjectDefiner() {

			Assert.state(this.schemaObjectDefiner != null,
				"SchemaObjectDefiner was not initialized");

			return this.schemaObjectDefiner;
		}

		protected SchemaObjectContext with(GemfireAdminOperations gemfireAdminOperations) {
			Assert.notNull(gemfireAdminOperations, "GemfireAdminOperations are required");
			this.gemfireAdminOperations = gemfireAdminOperations;
			return this;
		}

		protected SchemaObjectContext with(SchemaObjectCollector schemaObjectCollector) {
			Assert.notNull(schemaObjectCollector, "SchemaObjectCollector is required");
			this.schemaObjectCollector = schemaObjectCollector;
			return this;
		}

		protected SchemaObjectContext with(SchemaObjectDefiner schemaObjectDefiner) {
			Assert.notNull(schemaObjectDefiner, "SchemaObjectDefiner is required");
			this.schemaObjectDefiner = schemaObjectDefiner;
			return this;
		}
	}
}
