/*
 * Copyright 2017-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.admin.remote;

import java.net.URI;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.execute.Function;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.config.schema.definitions.IndexDefinition;
import org.springframework.data.gemfire.config.schema.definitions.RegionDefinition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
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
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.execute.Function
 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
 * @see org.springframework.data.gemfire.config.admin.remote.FunctionGemfireAdminTemplate
 * @since 2.0.0
 */
public class RestHttpGemfireAdminTemplate extends FunctionGemfireAdminTemplate {

	protected static final boolean CREATE_REGION_SKIP_IF_EXISTS_DEFAULT = true;

	protected static final int DEFAULT_PORT = 7070;

	protected static final String DEFAULT_HOST = "localhost";

	protected static final String MANAGEMENT_REST_API_URL_TEMPLATE = "http://%1$s:%2$d/gemfire/v1/";

	private final RestOperations restTemplate;

	private final String managementRestApiUrl;

	/**
	 * Constructs an instance of the {@link RestHttpGemfireAdminTemplate} initialized with
	 * the given {@link ClientCache} and configured with the default host and port when accessing
	 * the GemFire or Geode Management REST API interface.
	 *
	 * @param clientCache reference to the {@link ClientCache}
	 * @throws IllegalArgumentException if the {@link ClientCache} reference is {@literal null}.
	 * @see org.apache.geode.cache.client.ClientCache
	 */
	public RestHttpGemfireAdminTemplate(ClientCache clientCache) {
		this(clientCache, DEFAULT_HOST, DEFAULT_PORT);
	}

	/**
	 * Constructs an instance of the {@link RestHttpGemfireAdminTemplate} initialized with
	 * the given {@link ClientCache} and configured with the specified host and port when accessing
	 * the GemFire or Geode Management REST API interface.
	 *
	 * @param clientCache reference to the {@link ClientCache}
	 * @param host {@link String} containing the hostname of the GemFire/Geode Manager.
	 * @param port integer value specifying the port on which the GemFire/Geode Manager HTTP Service is listening
	 * for HTTP clients.
	 * @throws IllegalArgumentException if the {@link ClientCache} reference is {@literal null}.
	 * @see org.apache.geode.cache.client.ClientCache
	 */
	public RestHttpGemfireAdminTemplate(ClientCache clientCache, String host, int port) {

		super(clientCache);

		this.restTemplate = newRestOperations();
		this.managementRestApiUrl = resolveManagementRestApiUrl(host, port);
	}

	/**
	 * Constructs a new instance of the Spring {@link RestTemplate} to perform REST API operations over HTTP.
	 *
	 * @return a new instance of Spring's {@link RestTemplate}.
	 * @see org.springframework.http.client.SimpleClientHttpRequestFactory
	 * @see org.springframework.web.client.RestOperations
	 * @see org.springframework.web.client.RestTemplate
	 */
	RestOperations newRestOperations() {
		return new RestTemplate(new SimpleClientHttpRequestFactory());
	}

	/**
	 * Resolves the Pivotal GemFire or Apache Geode Management REST API URL given the host and port
	 * of the GemFire/Geode Manager's embedded HTTP service.
	 *
	 * @param host {@link String} containing the hostname of the Manager running the embedded HTTP service
	 * and Management REST API.
	 * @param port integer specifying the port that the embedded Manager's HTTP service is listening on.
	 * @return the resolved URL.
	 */
	private String resolveManagementRestApiUrl(String host, int port) {
		return String.format(MANAGEMENT_REST_API_URL_TEMPLATE, host, port);
	}

	/**
	 * Returns a reference to the resolved GemFire/Geode Management REST API URL.
	 *
	 * @return a {@link String} containing the resolved GemFire/Geode Management REST API URL.
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
	protected RestOperations getRestOperations() {
		return this.restTemplate;
	}

	@Override
	public void createIndex(IndexDefinition indexDefinition) {

		HttpHeaders httpHeaders = new HttpHeaders();

		httpHeaders.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

		// HTTP Message Body
		MultiValueMap<String, Object> requestParameters = new LinkedMultiValueMap<>();

		requestParameters.add("name", indexDefinition.getName());
		requestParameters.add("expression", indexDefinition.getExpression());
		requestParameters.add("region", indexDefinition.getFromClause());
		requestParameters.add("type", indexDefinition.getIndexType().toString());

		RequestEntity<MultiValueMap<String, Object>> requestEntity =
			new RequestEntity<>(requestParameters, httpHeaders, HttpMethod.POST, resolveCreateIndexUri());

		ResponseEntity<String> response = getRestOperations().exchange(requestEntity, String.class);

		// TODO do something with result; e.g. log when failure (or when not "OK")
		HttpStatus.OK.equals(response.getStatusCode());
	}

	protected URI resolveCreateIndexUri() {
		return URI.create(getManagementRestApiUrl().concat("indexes"));
	}

	@Override
	public void createRegion(RegionDefinition regionDefinition) {

		HttpHeaders httpHeaders = new HttpHeaders();

		httpHeaders.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

		// HTTP Message Body
		MultiValueMap<String, Object> requestParameters = new LinkedMultiValueMap<>();

		requestParameters.add("name", regionDefinition.getName());
		requestParameters.add("type", regionDefinition.getRegionShortcut().toString());
		requestParameters.add("skip-if-exists", String.valueOf(CREATE_REGION_SKIP_IF_EXISTS_DEFAULT));

		RequestEntity<MultiValueMap<String, Object>> requestEntity =
			new RequestEntity<>(requestParameters, httpHeaders, HttpMethod.POST, resolveCreateRegionUri());

		ResponseEntity<String> response = getRestOperations().exchange(requestEntity, String.class);

		// TODO do something with result; e.g. log when failure (or when not "OK")
		HttpStatus.OK.equals(response.getStatusCode());
	}

	protected URI resolveCreateRegionUri() {
		return URI.create(getManagementRestApiUrl().concat("regions"));
	}
}
