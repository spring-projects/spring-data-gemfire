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
package org.springframework.data.gemfire.config.admin.remote;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.execute.Function;

import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.config.schema.definitions.IndexDefinition;
import org.springframework.data.gemfire.config.schema.definitions.RegionDefinition;
import org.springframework.data.gemfire.config.support.RestTemplateConfigurer;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.data.gemfire.util.NetworkUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.ClientHttpRequestFactory;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.util.Assert;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StringUtils;
import org.springframework.web.client.RestOperations;
import org.springframework.web.client.RestTemplate;

/**
 * {@link RestHttpGemfireAdminTemplate} is class implementing the {@link GemfireAdminOperations} interface,
 * extending the {@link FunctionGemfireAdminTemplate} to support administrative (management) operations
 * on a Pivotal GemFire or Apache Geode cluster using the Management REST API interface over HTTP.
 *
 * The fallback is using {@link Function} execution if the a particular administrative (management) operation
 * is not supported or has not been implemented against the Management REST API interface over HTTP.
 *
 * @author John Blum
 * @see java.net.HttpURLConnection
 * @see java.net.URI
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.execute.Function
 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
 * @see org.springframework.data.gemfire.config.admin.remote.FunctionGemfireAdminTemplate
 * @see org.springframework.http.HttpHeaders
 * @see org.springframework.http.HttpMethod
 * @see org.springframework.http.HttpStatus
 * @see org.springframework.http.RequestEntity
 * @see org.springframework.http.ResponseEntity
 * @see org.springframework.http.client.ClientHttpRequestFactory
 * @see org.springframework.http.client.ClientHttpRequestInterceptor
 * @see org.springframework.http.client.SimpleClientHttpRequestFactory
 * @see org.springframework.web.client.RestOperations
 * @see org.springframework.web.client.RestTemplate
 * @since 2.0.0
 */
public class RestHttpGemfireAdminTemplate extends FunctionGemfireAdminTemplate {

	protected static final boolean DEFAULT_CREATE_REGION_SKIP_IF_EXISTS = true;
	protected static final boolean DEFAULT_HTTP_FOLLOW_REDIRECTS = true;

	// Default port to -1 to let HTTP clients determine the port from the protocol/scheme.
	// By default, Apache Geode / Pivotal GemFire's (embedded) HTTP service listens on port 7070.
	protected static final int DEFAULT_PORT = -1;

	protected static final String DEFAULT_HOST = "localhost";
	protected static final String DEFAULT_SCHEME = "https";
	protected static final String HTTP_SCHEME = "http";
	protected static final String HTTPS_SCHEME = "https";
	protected static final String MANAGEMENT_REST_API_URL_TEMPLATE = "%1$s://%2$s:%3$d/gemfire/v1";
	protected static final String MANAGEMENT_REST_API_NO_PORT_URL_TEMPLATE = "%1$s://%2$s/gemfire/v1";

	protected static final List<String> VALID_SCHEMES = Arrays.asList(HTTP_SCHEME, HTTPS_SCHEME);

	private final RestOperations restTemplate;

	private final String managementRestApiUrl;

	/**
	 * Constructs a new instance of {@link RestHttpGemfireAdminTemplate} initialized with the given {@link ClientCache}
	 * and configured with the default HTTP schema, host and port when accessing the Apache Geode or Pivotal GemFire
	 * Management REST API interface.
	 *
	 * @param clientCache reference to the {@link ClientCache}.
	 * @throws IllegalArgumentException if {@link ClientCache} is {@literal null}.
	 * @see #RestHttpGemfireAdminTemplate(ClientCache, String, String, int, boolean, List)
	 * @see org.apache.geode.cache.client.ClientCache
	 */
	public RestHttpGemfireAdminTemplate(ClientCache clientCache) {

		this(clientCache, DEFAULT_SCHEME, DEFAULT_HOST, DEFAULT_PORT, DEFAULT_HTTP_FOLLOW_REDIRECTS,
			Collections.emptyList());
	}

	/**
	 * Constructs a new instance of {@link RestHttpGemfireAdminTemplate} initialized with the given {@link ClientCache}
	 * and configured with the specified HTTP scheme, host, port, redirects and
	 * {@link ClientHttpRequestInterceptor ClientHttpRequestInterceptors}
	 * when accessing the Apache Geode or Pivotal GemFire Management REST API interface.
	 *
	 * @param clientCache reference to the {@link ClientCache}
	 * @param scheme {@link String} specifying the HTTP scheme to use (e.g. HTTP or HTTPS).
	 * @param host {@link String} containing the hostname of the GemFire/Geode Manager.
	 * @param port integer value specifying the port on which the GemFire/Geode Manager HTTP Service is listening
	 * for HTTP clients.
	 * @param followRedirects boolean indicating whether HTTP Redirects (with HTTP Status Code 3xx) should be followed.
	 * @param clientHttpRequestInterceptors {@link List} of {@link ClientHttpRequestInterceptor} used to intercept
	 * and decorate the HTTP request and HTTP response.
	 * @throws IllegalArgumentException if the {@link ClientCache} reference is {@literal null}.
	 * @see org.springframework.http.client.ClientHttpRequestInterceptor
	 * @see org.apache.geode.cache.client.ClientCache
	 */
	public RestHttpGemfireAdminTemplate(ClientCache clientCache, String scheme, String host, int port,
			boolean followRedirects, List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors) {

		this(clientCache, scheme, host, port, followRedirects, clientHttpRequestInterceptors, Collections.emptyList());
	}

	/**
	 * Constructs a new instance of {@link RestHttpGemfireAdminTemplate} initialized with the given {@link ClientCache}
	 * and configured with the specified HTTP scheme, host, port, redirects and
	 * {@link ClientHttpRequestInterceptor ClientHttpRequestInterceptors}
	 * when accessing the Apache Geode or Pivotal GemFire Management REST API interface.
	 *
	 * @param clientCache reference to the {@link ClientCache}
	 * @param scheme {@link String} specifying the HTTP scheme to use (e.g. HTTP or HTTPS).
	 * @param host {@link String} containing the hostname of the GemFire/Geode Manager.
	 * @param port integer value specifying the port on which the GemFire/Geode Manager HTTP Service is listening
	 * for HTTP clients.
	 * @param followRedirects boolean indicating whether HTTP Redirects (with HTTP Status Code 3xx) should be followed.
	 * @param clientHttpRequestInterceptors {@link List} of {@link ClientHttpRequestInterceptor} used to intercept
	 * and decorate the HTTP request and HTTP response.
	 * @throws IllegalArgumentException if the {@link ClientCache} reference is {@literal null}.
	 * @see org.apache.geode.cache.client.ClientCache
	 * @see org.springframework.data.gemfire.config.support.RestTemplateConfigurer
	 * @see org.springframework.http.client.ClientHttpRequestInterceptor
	 * @see #newClientHttpRequestFactory(boolean)
	 * @see #newRestOperations(ClientHttpRequestFactory, List, List)
	 * @see #resolveManagementRestApiUrl(String, String, int)
	 */
	public RestHttpGemfireAdminTemplate(ClientCache clientCache, String scheme, String host, int port,
			boolean followRedirects, List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors,
			List<RestTemplateConfigurer> restTemplateConfigurers) {

		super(clientCache);

		ClientHttpRequestFactory clientHttpRequestFactory = newClientHttpRequestFactory(followRedirects);

		this.managementRestApiUrl = resolveManagementRestApiUrl(scheme, host, port);

		this.restTemplate =
			newRestOperations(clientHttpRequestFactory, clientHttpRequestInterceptors, restTemplateConfigurers);
	}

	/**
	 * Constructs a new instance of {@link ClientHttpRequestFactory} to make HTTP client requests.
	 *
	 * @param followRedirects boolean value indicating whether HTTP redirects (with HTTP Status Code 3xx)
	 * should be followed.
	 * @return a new {@link ClientHttpRequestFactory}.
	 * @see org.springframework.http.client.ClientHttpRequestFactory
	 */
	@SuppressWarnings("unchecked")
	protected <T extends ClientHttpRequestFactory> T newClientHttpRequestFactory(boolean followRedirects) {
		return (T) new FollowRedirectsSimpleClientHttpRequestFactory(followRedirects);
	}

	/**
	 * Constructs a new instance of the Spring {@link RestTemplate} to perform REST API operations over HTTP.
	 *
	 * @param clientHttpRequestFactory {@link ClientHttpRequestFactory} used to construct HTTP request objects.
	 * @param clientHttpRequestInterceptors {@link List} of {@link ClientHttpRequestInterceptor} used to intercept
	 * and decorate the HTTP request and HTTP response.
	 * @return a new instance of Spring's {@link RestTemplate}.
	 * @see org.springframework.http.client.ClientHttpRequestInterceptor
	 * @see org.springframework.http.client.SimpleClientHttpRequestFactory
	 * @see org.springframework.web.client.RestOperations
	 * @see org.springframework.web.client.RestTemplate
	 */
	@SuppressWarnings("unchecked")
	protected <T extends RestOperations> T newRestOperations(ClientHttpRequestFactory clientHttpRequestFactory,
			List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors,
			List<RestTemplateConfigurer> restTemplateConfigurers) {

		RestTemplate restTemplate = new RestTemplate(clientHttpRequestFactory);

		Optional.ofNullable(clientHttpRequestInterceptors)
			.ifPresent(restTemplate.getInterceptors()::addAll);

		CollectionUtils.nullSafeList(restTemplateConfigurers).stream()
			.filter(Objects::nonNull)
			.forEach(configurer -> configurer.configure(restTemplate));

		return (T) restTemplate;
	}

	/**
	 * Resolves the Pivotal GemFire or Apache Geode Management REST API URL given the host and port
	 * of the Pivotal GemFire/Apache Geode Manager's embedded HTTP service.
	 *
	 * @param host {@link String} containing the hostname of the Manager running the embedded HTTP service
	 * and Management REST API.
	 * @param port integer specifying the port that the embedded Manager's HTTP service is listening on.
	 * @return the resolved URL.
	 */
	String resolveManagementRestApiUrl(String scheme, String host, int port) {

		return NetworkUtils.isValidNonEphemeralPort(port)
			? String.format(MANAGEMENT_REST_API_URL_TEMPLATE, scheme, host, port)
			: String.format(MANAGEMENT_REST_API_NO_PORT_URL_TEMPLATE, scheme, host);
	}

	/**
	 * Returns a reference to the resolved Pivotal GemFire/Apache Geode Management REST API URL.
	 *
	 * @return a {@link String} containing the resolved Pivotal GemFire/Apache Geode Management REST API URL.
	 */
	protected String getManagementRestApiUrl() {
		return this.managementRestApiUrl;
	}

	/**
	 * Returns a reference to the {@link RestOperations} used to perform REST API calls.
	 *
	 * @return a reference to the {@link RestOperations} used to perform REST API calls.
	 * @see org.springframework.web.client.RestOperations
	 */
	@SuppressWarnings("unchecked")
	protected <T extends RestOperations> T getRestOperations() {
		return (T) this.restTemplate;
	}

	@Override
	public void createIndex(IndexDefinition indexDefinition) {

		HttpHeaders httpHeaders = new HttpHeaders();

		httpHeaders.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

		// HTTP Message Body
		MultiValueMap<String, Object> httpRequestParameters = new LinkedMultiValueMap<>();

		httpRequestParameters.add("name", indexDefinition.getName());
		httpRequestParameters.add("expression", indexDefinition.getExpression());
		httpRequestParameters.add("region", indexDefinition.getFromClause());
		httpRequestParameters.add("type", indexDefinition.getIndexType().toString());

		RequestEntity<MultiValueMap<String, Object>> requestEntity =
			new RequestEntity<>(httpRequestParameters, httpHeaders, HttpMethod.POST, resolveCreateIndexUri());

		ResponseEntity<String> response = getRestOperations().exchange(requestEntity, String.class);

		// TODO do something with the result; e.g. log when failure (or when not "OK")
		HttpStatus.OK.equals(response.getStatusCode());
	}

	protected URI resolveCreateIndexUri() {
		return URI.create(getManagementRestApiUrl().concat("/indexes"));
	}

	@Override
	public void createRegion(RegionDefinition regionDefinition) {

		HttpHeaders httpHeaders = new HttpHeaders();

		httpHeaders.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

		// HTTP Message Body
		MultiValueMap<String, Object> httpRequestParameters = new LinkedMultiValueMap<>();

		httpRequestParameters.add("name", regionDefinition.getName());
		httpRequestParameters.add("type", regionDefinition.getRegionShortcut().toString());
		httpRequestParameters.add("skip-if-exists", String.valueOf(DEFAULT_CREATE_REGION_SKIP_IF_EXISTS));

		RequestEntity<MultiValueMap<String, Object>> requestEntity =
			new RequestEntity<>(httpRequestParameters, httpHeaders, HttpMethod.POST, resolveCreateRegionUri());

		ResponseEntity<String> response = getRestOperations().exchange(requestEntity, String.class);

		// TODO do something with the result; e.g. log when failure (or when not "OK")
		HttpStatus.OK.equals(response.getStatusCode());
	}

	protected URI resolveCreateRegionUri() {
		return URI.create(getManagementRestApiUrl().concat("/regions"));
	}

	public static class Builder {

		private boolean followRedirects = DEFAULT_HTTP_FOLLOW_REDIRECTS;

		private int port = DEFAULT_PORT;

		private ClientCache clientCache;

		private final List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors = new ArrayList<>();
		private final List<RestTemplateConfigurer> restTemplateConfigurers = new ArrayList<>();

		private String hostname = DEFAULT_HOST;
		private String scheme = DEFAULT_SCHEME;

		public Builder followRedirects(boolean followRedirects) {
			this.followRedirects = followRedirects;
			return this;
		}

		public Builder listenOn(int port) {

			Assert.isTrue(NetworkUtils.isValidNonEphemeralPort(port),
				String.format(NetworkUtils.INVALID_NO_EPHEMERAL_PORT_MESSAGE, port));

			this.port = port;

			return this;
		}

		public Builder on(String hostname) {
			this.hostname = StringUtils.hasText(hostname) ? hostname : DEFAULT_HOST;
			return this;
		}

		public Builder using(String scheme) {

			scheme = String.valueOf(scheme).trim().toLowerCase();

			Assert.isTrue(VALID_SCHEMES.contains(scheme),
				String.format("Scheme [%s] is not valid; must be 1 of %s", scheme, VALID_SCHEMES));

			this.scheme = scheme;

			return this;
		}

		public Builder with(ClientCache clientCache) {
			this.clientCache = clientCache;
			return this;
		}

		/**
		 * @deprecated use {@link #withInterceptors(ClientHttpRequestInterceptor...)}.
		 */
		@Deprecated
		public Builder with(ClientHttpRequestInterceptor... clientHttpRequestInterceptors) {
			return withInterceptors(clientHttpRequestInterceptors);
		}

		/**
		 * @deprecated use {@link #withInterceptors(List)}.
		 */
		@Deprecated
		public Builder with(List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors) {
			return withInterceptors(clientHttpRequestInterceptors);
		}

		public Builder withConfigurers(RestTemplateConfigurer... restTemplateConfigurers) {

			List<RestTemplateConfigurer> restTemplateConfigurerList =
				Arrays.asList(ArrayUtils.nullSafeArray(restTemplateConfigurers, RestTemplateConfigurer.class));

			return withConfigurers(restTemplateConfigurerList);
		}

		public Builder withConfigurers(List<RestTemplateConfigurer> restTemplateConfigurers) {

			this.restTemplateConfigurers.addAll(CollectionUtils.nullSafeList(restTemplateConfigurers));

			return this;
		}

		public Builder withInterceptors(ClientHttpRequestInterceptor... clientHttpRequestInterceptors) {

			List<ClientHttpRequestInterceptor> clientHttpRequestInterceptorList =
				Arrays.asList(ArrayUtils.nullSafeArray(clientHttpRequestInterceptors,
					ClientHttpRequestInterceptor.class));

			return withInterceptors(clientHttpRequestInterceptorList);
		}

		public Builder withInterceptors(List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors) {

			this.clientHttpRequestInterceptors.addAll(CollectionUtils.nullSafeList(clientHttpRequestInterceptors));

			return this;
		}

		public RestHttpGemfireAdminTemplate build() {

			return new RestHttpGemfireAdminTemplate(this.clientCache, this.scheme, this.hostname, this.port,
				this.followRedirects, this.clientHttpRequestInterceptors, this.restTemplateConfigurers);
		}
	}

	public static class FollowRedirectsSimpleClientHttpRequestFactory extends SimpleClientHttpRequestFactory {

		private final boolean followRedirects;

		public FollowRedirectsSimpleClientHttpRequestFactory(boolean followRedirects) {
			this.followRedirects = followRedirects;
		}

		public boolean isFollowRedirects() {
			return this.followRedirects;
		}

		@Override
		protected void prepareConnection(HttpURLConnection connection, String httpMethod) throws IOException {

			super.prepareConnection(connection, httpMethod);

			connection.setInstanceFollowRedirects(isFollowRedirects());
		}
	}
}
