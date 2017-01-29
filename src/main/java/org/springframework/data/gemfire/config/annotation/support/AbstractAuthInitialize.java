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

package org.springframework.data.gemfire.config.annotation.support;

import java.util.Optional;
import java.util.Properties;

import org.apache.geode.LogWriter;
import org.apache.geode.distributed.DistributedMember;
import org.apache.geode.security.AuthInitialize;
import org.apache.geode.security.AuthenticationFailedException;
import org.springframework.context.EnvironmentAware;
import org.springframework.core.env.Environment;
import org.springframework.data.gemfire.support.WiringDeclarableSupport;

/**
 * The {@link AbstractAuthInitialize} class is an abstract support class and basic implementation
 * of the {@link AuthInitialize} interface used in the authentication of a client or peer
 * with a secure GemFire/Geode cluster.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.apache.geode.security.AuthInitialize
 * @see org.springframework.context.EnvironmentAware
 * @see org.springframework.core.env.Environment
 * @see org.springframework.data.gemfire.support.WiringDeclarableSupport
 * @since 2.0.0
 */
public abstract class AbstractAuthInitialize extends WiringDeclarableSupport
		implements AuthInitialize, EnvironmentAware {

	private Environment environment;

	/**
	 * Sets a reference to the configured Spring {@link Environment}.
	 *
	 * @param environment reference to the configured Spring {@link Environment}.
	 * @see org.springframework.core.env.Environment
	 */
	@Override
	@SuppressWarnings("all")
	public void setEnvironment(Environment environment) {
		this.environment = environment;
	}

	/**
	 * Returns a reference to the configured Spring {@link Environment}.
	 *
	 * @return a reference to the configured Spring {@link Environment}.
	 * @see org.springframework.core.env.Environment
	 */
	protected Optional<Environment> getEnvironment() {
		return Optional.ofNullable(this.environment);
	}

	/* (non-Javadoc */
	@Override
	@SuppressWarnings("deprecation")
	public final void init(LogWriter logWriter, LogWriter logWriter1) throws AuthenticationFailedException {
		doInit();
	}

	protected void doInit() {
	}

	/* (non-Javadoc */
	@Override
	public final Properties getCredentials(Properties properties, DistributedMember distributedMember, boolean isPeer)
			throws AuthenticationFailedException {

		return doGetCredentials(properties);
	}

	protected abstract Properties doGetCredentials(Properties properties);

	/* (non-Javadoc */
	@Override
	public void close() {
	}
}
