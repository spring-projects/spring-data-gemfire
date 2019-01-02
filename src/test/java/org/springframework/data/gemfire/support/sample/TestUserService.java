/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.support.sample;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

/**
 * The TestUserService class is an implementation of the UserService service interface for performing service operations
 * on Users.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.repository.sample.User
 * @see org.springframework.data.gemfire.support.sample.UserService
 * @see org.springframework.stereotype.Service
 * @since 1.4.0
 */
@Service("userService")
@SuppressWarnings("unused")
public class TestUserService implements UserService {

	@Autowired
	private UserDao userDao;

	public UserDao getUserDao() {
		Assert.state(userDao != null, "A reference to the UserDao was not properly configured!");
		return userDao;
	}
}
